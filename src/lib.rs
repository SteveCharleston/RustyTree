//! This crate provides a library to render a graphical representation of a filesystem in a tree
//! like fashion.
//!
//! Each directory is entered in an own thread to increase speed when concurrently traversing the
//! directory tree. Additionally expensive computations can be done like the hash sum generation of
//! files, which will also be done in a separate thread per file.

#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use ansi_term::Color;
use clap::Parser;
use lscolors::{LsColors, Style};
use rayon::prelude::*;
use std::fs::{DirEntry, Metadata};
use std::os::unix::prelude::MetadataExt;
use std::path::{Path, PathBuf};
use std::{fmt, fs, io};

#[derive(Debug, Default, Parser)]
#[clap(
    about = "List contents of directories in a tree-like format",
    author = "Steven Schalhorn <steven@schalhorn.org>"
)]
/// Arguments to the application.
pub struct Options {
    #[clap(default_value = ".", value_parser)]
    /// Path to the directory to traverse into
    pub path: String,

    #[clap(short = 'F', long)]
    /// Append a `/' for directories
    pub classify: bool,

    #[clap(short = 'n', long)]
    /// Turn colorization off
    pub nocolor: bool,

    #[clap(short = 'f', long)]
    /// Print the full path prefix for each file
    pub fullpath: bool,

    #[clap(short = 'a', long)]
    /// Print hidden files as well
    pub all: bool,

    #[clap(short = 'd', long)]
    /// List directories only
    pub dironly: bool,

    #[clap(short = 'L', long)]
    /// Maximal depth of the directory tree
    pub level: Option<usize>,

    #[clap(short = 'p', long)]
    /// Print the group name or GID
    pub protections: bool,

    #[clap(short = 'g', long)]
    /// Print the group name or GID
    pub group: bool,

    #[clap(short = 'u', long)]
    /// Print the username name or UID
    pub user: bool,

    #[clap(long)]
    /// Omit the file and directory report at the end of the tree
    pub noreport: bool,
}

/// Indentation if no parent exists
const INDENT_SIGN: &str = "  ";

/// Bar if a parent exists
const TREE_SIGN: &str = "│ ";

/// In front of a file or dir if it is not the last
const INNER_BRANCH: &str = "├─";

/// In front of a file or dir if it is the last
const FINAL_BRANCH: &str = "└─";

/// Represent the different possible indentation components of a file.
#[derive(Clone, Debug)]
enum TreeLevel {
    /// Indentation if no parent exists
    Indent,
    /// Bar if a parent exists
    TreeBar,
    /// In front of a file or dir if it is not the last
    TreeBranch,
    /// In front of a file or dir if it is the last
    TreeFinalBranch,
}

/// Represent a file with all necessary accompanying metadata.
#[derive(Clone, Debug)]
struct TreeEntry {
    /// Name of the current file or directory
    name: PathBuf,

    /// Save kind of entry to display it differently
    kind: TreeEntryKind,

    /// Metadata about the file or directory
    meta: TreeLevelMeta,

    /// List of different levels of parent directories up to the root
    levels: Vec<TreeLevel>,

    /// List of child entries to enable a recursive data structure
    children: TreeChild,
}

impl TreeEntry {
    /// Calculate the length of the longest field.
    ///
    /// Goes through the whole tree and subtrees and looks at the given field for every node to
    /// determine the length of the longest entry. Return those length to enable better formatting
    /// with this information.
    fn longest_fieldentry(
        &self,
        get_field: &(dyn Fn(&TreeEntry) -> String + Send + Sync),
    ) -> usize {
        let field_length = get_field(self).len(); // if conversation overflows, only
                                                  // formatting will be affected

        // maybe a match would be better suited, but this looks cleaner
        let child_max = if let TreeChild::Children(children) = &self.children {
            children
                .par_iter()
                .map(|child| child.longest_fieldentry(&get_field))
                .max()
                .unwrap_or_default()
        } else {
            0
        };

        field_length.max(child_max)
    }
}

#[derive(Clone, Default, Debug)]
/// Store metadata about a TreeEntry.
struct TreeLevelMeta {
    /// Chmods of file or directory
    chmods: Option<u32>,

    /// Username of file or directory
    user: Option<String>,

    /// Group name of file or directory
    group: Option<String>,
}

impl TreeLevelMeta {
    /// Generate a new TreeLevelMeta from the given data metadata
    fn from(meta: &Metadata, options: &Options) -> TreeLevelMeta {
        let chmods = if options.protections {
            Some(meta.mode())
        } else {
            None
        };

        let user = if options.user {
            Some(
                users::get_user_by_uid(meta.uid())
                    .unwrap()
                    .name()
                    .to_string_lossy()
                    .to_string(),
            )
        } else {
            None
        };

        let group = if options.group {
            Some(
                users::get_group_by_gid(meta.gid())
                    .unwrap()
                    .name()
                    .to_string_lossy()
                    .to_string(),
            )
        } else {
            None
        };

        TreeLevelMeta {
            chmods,
            user,
            group,
        }
    }
}

/// Cache the lengths of some TreeEntry fields to avoid recalculating them during drawing.
#[derive(Default)]
struct TreeEntryLengths {
    /// Length of the longest user field
    user: usize,

    /// Length of the longest group field
    group: usize,
}

/// Hold the rendered tree as well as number of directories and files to generate the final status
/// line.
struct TreeRepresentation {
    /// Final rendered string representation of the tree
    rendered: String,
    /// Number of directories in the rendered tree
    directories: u32,
    /// Number of files in the rendered tree
    files: u32,
}

impl fmt::Display for TreeRepresentation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}\n\n{} directories, {} files",
            self.rendered.trim(),
            self.directories,
            self.files,
        )
    }
}

/// Represent the possible states of a subdirectory or alternatives in case of error or no dir.
#[derive(Clone, Debug)]
enum TreeChild {
    /// No children exist or have been read yet
    None,
    /// Directory couldn't be accessed for some reason
    Error(io::ErrorKind),
    /// The expected child entries
    Children(Vec<TreeEntry>),
}

/// Represent which kind of file a TreeEntry is.
#[derive(Clone, Debug)]
enum TreeEntryKind {
    /// TreeEntry is a regular file
    File,
    /// TreeEntry is a Directory
    Directory,
}

/// Read the directory for the given Path and sort the files alphabetically.
///
/// Collect all entries in the given directory and sort them in alphabetical order. Do this in a
/// case insensitive manner.
///
/// # Errors
///
/// Will return an error in the following situations, but not limited to:
/// * The provided `path` doesn't exist.
/// * The process lacks permissions to view the contens.
/// * The `path` points at a non-directory file.
fn read_dir(path: &impl AsRef<Path>, options: &Options) -> Result<Vec<DirEntry>, io::Error> {
    // dbg!(PathBuf::from(path.as_ref()));
    let mut paths: Vec<_> = fs::read_dir(path)?
        .map(|r| r.expect("Reading file inside a directory")) // not expected to normally fail
        .filter(|r| options.all || !r.file_name().to_string_lossy().starts_with('.')) // hidden
        .filter(|r| !options.dironly || r.path().is_dir()) // directories only
        .collect();

    paths.sort_by_key(|entry| entry.path().to_string_lossy().to_lowercase());
    Ok(paths)
}

/// Extracts a String from an Option or returns Emptry string if None.
fn string_from_opt_field(field: &Option<String>) -> String {
    field.as_ref().unwrap_or(&"".to_string()).to_string()
}

/// Render the given TreeEntry into a string representation.
fn render_tree_level(
    entry: &TreeEntry,
    options: &Options,
    sizes: &TreeEntryLengths,
    lscolors: &LsColors,
) -> String {
    let style = lscolors.style_for_path(&entry.name);
    let ansi_style = style.map(Style::to_ansi_term_style).unwrap_or_default();

    let mut rendered_entry = String::new();
    rendered_entry += "\n";

    if let Some(chmods) = &entry.meta.chmods {
        rendered_entry += format!("{} ", unix_mode::to_string(*chmods).as_str()).as_str();
    }

    if let Some(user) = &entry.meta.user {
        rendered_entry += format!("{:width$}", user, width = sizes.user + 1).as_str();
    }

    if let Some(group) = &entry.meta.group {
        rendered_entry += format!("{:width$}", group, width = sizes.group + 1).as_str();
    }

    for level in &entry.levels {
        let current_level = match level {
            TreeLevel::Indent => INDENT_SIGN.to_string(),
            TreeLevel::TreeBar => TREE_SIGN.to_string() + INDENT_SIGN,
            TreeLevel::TreeBranch => INNER_BRANCH.to_string(),
            TreeLevel::TreeFinalBranch => FINAL_BRANCH.to_string(),
        };
        rendered_entry += current_level.as_str();
    }

    // first entry should always be displayed as given, for the rest only the filename if fullpath
    // option is not given.
    if entry.levels.is_empty() || options.fullpath {
        rendered_entry += ansi_style
            .paint(entry.name.to_string_lossy())
            .to_string()
            .as_str()
    } else {
        rendered_entry += ansi_style
            .paint(entry.name.file_name().unwrap().to_string_lossy())
            .to_string()
            .as_str()
    };

    if let TreeEntryKind::Directory = entry.kind {
        if options.classify {
            rendered_entry += "/";
        }
    }

    if let TreeChild::Error(error_kind) = entry.children {
        // if we can't access a directory, show a childentry with an error message
        let error_message = format!(" [Cannot access directory: {error_kind}]");
        rendered_entry += "\n";
        // Don't add anything if target dir can't be accessed
        if !&entry.levels.is_empty() {
            // iterate over the levels but omit last entry to draw error children correctly
            for level in &entry.levels[..entry.levels.len().saturating_sub(1)] {
                // We will only ever face Indents or TreeBars here, so match isn't appropriate
                if let TreeLevel::Indent = level {
                    rendered_entry += INDENT_SIGN;
                } else if let TreeLevel::TreeBar = level {
                    rendered_entry += format!("{TREE_SIGN}{INDENT_SIGN}").as_str();
                }
            }

            // if parent is last, don't add anything, otherwise add TREE_SIGN to connect next entry
            match entry.levels.last() {
                Some(TreeLevel::TreeFinalBranch) => {}
                _ => rendered_entry += TREE_SIGN,
            }
            rendered_entry += INDENT_SIGN;
        }

        rendered_entry += FINAL_BRANCH;

        if options.nocolor {
            rendered_entry += error_message.as_str();
        } else {
            rendered_entry += Color::Red.paint(error_message).to_string().as_str();
        }
    }
    rendered_entry
}

/// Generate a tree representation of the filesystem.
///
/// Walk the filesystem starting from the given directory and visit all child directories and files
/// recursively. Render all the files into a tree like string representation. Each directory visit
/// is done in a thread and also some expensive computations might be executed which will also be
/// threaded to distribute the load amongst the available cores.
pub fn tree(path: &impl AsRef<Path>, options: &Options) -> String {
    let indent_level: Vec<TreeLevel> = Vec::new();
    let lscolors = match options.nocolor {
        false => LsColors::from_env().unwrap_or_default(),
        true => LsColors::empty(),
    };

    let entry = TreeEntry {
        name: path.as_ref().to_path_buf(),
        kind: TreeEntryKind::Directory,
        levels: indent_level.clone(),
        children: recurse_paths(path, &indent_level, options),
        meta: TreeLevelMeta::from(&fs::metadata(path).unwrap(), options),
    };

    let sizes = TreeEntryLengths {
        user: entry.longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.user)),
        group: entry.longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.group)),
    };

    let full_representation = render_tree(&entry, options, &sizes, &lscolors);
    if !options.noreport {
        full_representation.to_string()
    } else {
        // without the report we only need the rendered tree directly
        full_representation.rendered.trim().to_string()
    }

    //format!("{:#?}", entry)
}

/// Generate a string representation of the nested TreeEntry data structure.
///
/// Walk the graph of the filesystem that has previously been generated and render it into an
/// actual String representation of a filesystem tree.
fn render_tree(
    tree_entry: &TreeEntry,
    options: &Options,
    sizes: &TreeEntryLengths,
    lscolors: &LsColors,
) -> TreeRepresentation {
    let mut current_level = render_tree_level(tree_entry, options, sizes, lscolors);
    let mut directories: u32 = 0;
    let mut files: u32 = 0;
    if let TreeChild::Children(children) = &tree_entry.children {
        for child in children {
            match child.kind {
                TreeEntryKind::Directory => directories += 1,
                TreeEntryKind::File => files += 1,
            }

            let sub_representation = render_tree(child, options, sizes, lscolors);
            current_level += sub_representation.rendered.as_str();
            directories += sub_representation.directories;
            files += sub_representation.files;
        }
    }

    TreeRepresentation {
        rendered: current_level,
        directories,
        files,
    }
}

/// Actually do the work of computing the tree.
fn recurse_paths(
    path: &impl AsRef<Path>,
    indent_level: &[TreeLevel],
    options: &Options,
) -> TreeChild {
    let entries = match read_dir(path, options) {
        Ok(entries) => entries,
        Err(io_err) => return TreeChild::Error(io_err.kind()),
    };
    // let entries = read_dir(path, options)?;
    let entries_len = entries.len();
    let output = entries
        .into_par_iter()
        .enumerate()
        .map(|(i, entry)| {
            let mut current_indent: Vec<TreeLevel> = indent_level.to_vec();
            let mut recurisve_indent: Vec<TreeLevel> = indent_level.to_vec();

            if i == entries_len - 1 {
                current_indent.push(TreeLevel::TreeFinalBranch);
                recurisve_indent.push(TreeLevel::Indent);
            } else {
                current_indent.push(TreeLevel::TreeBranch);
                recurisve_indent.push(TreeLevel::TreeBar);
            };

            let mut tree_entry = TreeEntry {
                name: entry.path(),
                kind: if entry.path().is_dir() {
                    TreeEntryKind::Directory
                } else {
                    TreeEntryKind::File
                },
                levels: current_indent.to_vec(),
                children: TreeChild::None,
                meta: TreeLevelMeta::from(&entry.metadata().unwrap(), options),
            };

            if entry.path().is_dir()
                && (options.level.is_none()
                    || options.level.unwrap() == 0
                    || options.level.unwrap() > indent_level.len() + 1)
            {
                // Either store the successfully retrieved subtree or store an error
                tree_entry.children = recurse_paths(&entry.path(), &recurisve_indent, options);
            }

            tree_entry
        })
        .collect::<Vec<TreeEntry>>();

    TreeChild::Children(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::fs::File;
    use std::os::unix::prelude::PermissionsExt;
    use tempfile;

    #[test]
    /// Verify that a generated filesystem tree is as expected with hidden files.
    fn test_print_paths_all() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_string_lossy().to_string(),
            nocolor: true,
            noreport: true,
            all: true,
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);

        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_output_standard_all()
            )
        );
    }

    #[test]
    /// Verify that a generated filesystem tree is as expected with only directories.
    fn test_print_paths_dironly() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_string_lossy().to_string(),
            nocolor: true,
            noreport: true,
            all: true,
            dironly: true,
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);

        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_output_standard_dironly_all()
            )
        );
    }

    #[test]
    /// Verify that a generated filesystem tree is as expected without hidden files.
    fn test_print_paths() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_string_lossy().to_string(),
            nocolor: true,
            noreport: true,
            all: false,
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);

        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_output_standard_nonhidden()
            )
        );
    }

    #[test]
    /// Verfiy that missing read permissions are handled gracefully.
    fn test_no_read_permissions() {
        let dir = create_directoy_no_permissions();
        let cli = Options {
            path: dir.path().to_string_lossy().to_string(),
            nocolor: true,
            noreport: true,
            ..Default::default()
        };

        let cli_colored = Options {
            path: dir.path().to_string_lossy().to_string(),
            nocolor: false,
            noreport: true,
            ..Default::default()
        };

        let expected_tree = "
└─root
  ├─does
  │   └─ [Cannot access directory: permission denied]
  ├─tres
  │   ├─ichi
  │   │   └─eins
  │   │     └─one
  │   │       └─ [Cannot access directory: permission denied]
  │   ├─ni
  │   │   ├─eins
  │   │   │   └─one
  │   │   │     └─ [Cannot access directory: permission denied]
  │   │   └─zwei
  │   │     └─two
  │   └─san
  │     └─zwei
  └─uno
    ├─bar.txt
    └─foo.txt";

        // Can't access root directory of tree
        let expected_root_error_tree = "/root/does
└─ [Cannot access directory: permission denied]";

        let out = tree(&dir.path(), &cli);
        let out_root_error = tree(&dir.path().join("root/does"), &cli);

        let out_root_error_colored = tree(&dir.path().join("root/does"), &cli_colored);

        println!("{}", out);
        println!("{}", out_root_error);
        println!("{}", out_root_error_colored);
        assert_eq!(
            out,
            format!("{}{}", dir.path().to_str().unwrap(), expected_tree)
        );
        assert_eq!(
            out_root_error,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_root_error_tree
            )
        );

        // Check that the colored error contains color reset character
        assert!(out_root_error_colored.ends_with("\u{1b}[0m"));
    }

    #[test]
    /// Verify that a directory is sorted correctly.
    fn test_read_dir_sorted() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        let cli = Options {
            nocolor: true,
            noreport: true,
            ..Default::default()
        };

        let sorted_dirs = ["Bardir", "does", "Foodir", "tres", "unos", "Xanadu"];

        fs::create_dir_all(dir).unwrap();

        fs::create_dir(dir.join("Foodir")).unwrap();
        fs::create_dir(dir.join("Bardir")).unwrap();
        fs::create_dir(dir.join("Xanadu")).unwrap();
        fs::create_dir(dir.join("unos")).unwrap();
        fs::create_dir(dir.join("does")).unwrap();
        fs::create_dir(dir.join("tres")).unwrap();

        let entries = read_dir(&dir, &cli).unwrap();

        for (i, entry) in entries.iter().enumerate() {
            println!("{:?} -- {}", entry.file_name(), sorted_dirs[i]);
            assert_eq!(entry.file_name().into_string().unwrap(), sorted_dirs[i]);
        }
    }

    #[test]
    /// Verify that tree entries from a list of `TreeLevel` enums are rendered correct.
    fn test_render_tree_entry() {
        let test_entries = vec![
            (vec![TreeLevel::TreeBranch], "\n├─filename"),
            (
                vec![TreeLevel::TreeBar, TreeLevel::TreeBranch],
                "\n│   ├─filename",
            ),
            (
                vec![TreeLevel::TreeBar, TreeLevel::TreeFinalBranch],
                "\n│   └─filename",
            ),
            (
                vec![TreeLevel::Indent, TreeLevel::TreeFinalBranch],
                "\n  └─filename",
            ),
            (
                vec![
                    TreeLevel::TreeBar,
                    TreeLevel::TreeBar,
                    TreeLevel::TreeBranch,
                ],
                "\n│   │   ├─filename",
            ),
            (
                vec![
                    TreeLevel::TreeBar,
                    TreeLevel::TreeBar,
                    TreeLevel::TreeFinalBranch,
                ],
                "\n│   │   └─filename",
            ),
        ];

        let cli = Options {
            path: ".".to_string(),
            nocolor: true,
            ..Default::default()
        };

        let lscolors = LsColors::default();
        let lengths = TreeEntryLengths::default();

        for (level_data, entry_presentation) in test_entries {
            let entry = TreeEntry {
                name: PathBuf::from("filename"),
                kind: TreeEntryKind::File,
                levels: level_data,
                children: TreeChild::None,
                meta: Default::default(),
            };
            assert_eq!(
                render_tree_level(&entry, &cli, &lengths, &lscolors),
                entry_presentation
            );
            print!("{}", render_tree_level(&entry, &cli, &lengths, &lscolors));
        }
    }

    #[test]
    /// Verify that directories are correctly classified with a trailing slash.
    fn test_render_tree_entry_classify() {
        let cli_classify = Options {
            path: ".".to_string(),
            classify: true,
            nocolor: true,
            ..Default::default()
        };

        let lscolors = LsColors::default();
        let lengths = TreeEntryLengths::default();

        let cli_classify_not = Options {
            path: ".".to_string(),
            nocolor: true,
            ..Default::default()
        };

        let direntry = TreeEntry {
            name: PathBuf::from("dirname"),
            kind: TreeEntryKind::Directory,
            levels: vec![TreeLevel::TreeBranch],
            children: TreeChild::None,
            meta: Default::default(),
        };

        let fileentry = TreeEntry {
            name: PathBuf::from("filename"),
            kind: TreeEntryKind::File,
            levels: vec![TreeLevel::TreeBranch],
            children: TreeChild::None,
            meta: Default::default(),
        };

        assert_eq!(
            render_tree_level(&direntry, &cli_classify, &lengths, &lscolors),
            "\n├─dirname/"
        );
        assert_eq!(
            render_tree_level(&direntry, &cli_classify_not, &lengths, &lscolors),
            "\n├─dirname"
        );

        assert_eq!(
            render_tree_level(&fileentry, &cli_classify, &lengths, &lscolors),
            "\n├─filename"
        );
        assert_eq!(
            render_tree_level(&fileentry, &cli_classify_not, &lengths, &lscolors),
            "\n├─filename"
        );
    }

    #[test]
    /// Verify that user and group lengths are taken from the lengths argument and that all options
    /// are displayed appropriately.
    fn test_render_tree_all_options() {
        let cli_group_user = Options {
            path: ".".to_string(),
            nocolor: true,
            protections: true,
            user: true,
            group: true,
            ..Default::default()
        };

        let lscolors = LsColors::default();
        let lengths_too_small = TreeEntryLengths { user: 0, group: 0 };
        let lengths_correct = TreeEntryLengths { user: 7, group: 8 };
        let lengths_too_big = TreeEntryLengths {
            user: 10,
            group: 20,
        };

        let fileentry = TreeEntry {
            name: PathBuf::from("filename"),
            kind: TreeEntryKind::File,
            levels: vec![TreeLevel::TreeBranch],
            children: TreeChild::None,
            meta: TreeLevelMeta {
                chmods: Some(0o100644),
                user: Some("foouser".to_string()),
                group: Some("foogroup".to_string()),
            },
        };
        assert_eq!(
            render_tree_level(&fileentry, &cli_group_user, &lengths_too_small, &lscolors),
            "\n-rw-r--r-- foouserfoogroup├─filename"
        );
        assert_eq!(
            render_tree_level(&fileentry, &cli_group_user, &lengths_correct, &lscolors),
            "\n-rw-r--r-- foouser foogroup ├─filename"
        );
        assert_eq!(
            render_tree_level(&fileentry, &cli_group_user, &lengths_too_big, &lscolors),
            "\n-rw-r--r-- foouser    foogroup             ├─filename"
        );
    }

    #[test]
    /// Verify that the nocolor option turns colorization off.
    fn test_render_tree_nocolor() {
        let dir = create_directory_tree();

        let cli_colorful = Options {
            path: ".".to_string(),
            nocolor: false,
            ..Default::default()
        };

        let cli_nocolor = Options {
            path: ".".to_string(),
            nocolor: true,
            ..Default::default()
        };

        assert!(tree(&dir, &cli_colorful).contains("[1;34m"),);
        assert!(!tree(&dir, &cli_nocolor).contains("[1;34m"),);
    }

    #[test]
    /// Verify that a full filepath is printed when this option is set.
    fn test_full_path() {
        let dir = create_directory_tree();

        let cli = Options {
            path: ".".to_string(),
            fullpath: true,
            ..Default::default()
        };

        let tree_out = tree(&dir, &cli);
        for filename in [
            "/Pictures/seasons/autumn.jpg",
            "/Trash/old/obsolete/does.md",
            "/Music/one.mp3",
        ] {
            assert!(tree_out.contains(filename));
        }
    }

    #[test]
    /// Verify that a correct report is generated
    fn test_tree_end_report() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();

        let cli = Options {
            ..Default::default()
        };

        let mut dir_counter: u32 = 0;
        let mut file_counter: u32 = 0;

        for dir_name in 0..=5 {
            dir_counter += 1;
            fs::create_dir_all(dir.join(dir_name.to_string())).unwrap();

            for file_name in 0..=10 {
                file_counter += 1;
                File::create(dir.join(format!("{}/{}", dir_name, file_name))).unwrap();
                assert!(tree(&dir, &cli).ends_with(
                    format!("{} directories, {} files", dir_counter, file_counter).as_str()
                ));
            }
        }
    }

    #[test]
    /// Verify that a deep nested tree can be limited by the level option.
    fn test_level_depth() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        let subdirs = vec!["one", "two", "three", "four", "five"];
        fs::create_dir_all(dir.join("one/two/three/four/five/")).unwrap();

        // skip first since level 0 is the full tree
        for (i, subdir) in subdirs.iter().enumerate().skip(1) {
            // Due to arrays starting with index 0 and the tree starting with level 1, we have to
            // increase the level by one.
            let cli_contains = Options {
                level: Some(i + 1),
                ..Default::default()
            };

            // Due to the circumstance mentioned above the level for the current index jsut not
            // contains the current directory.
            let cli_contains_not = Options {
                level: Some(i),
                ..Default::default()
            };

            assert!(tree(&dir, &cli_contains).contains(subdir));
            assert!(!tree(&dir, &cli_contains_not).contains(subdir));
        }

        // Check that a level of zero displasys the whole tree.
        let cli_zero = Options {
            level: Some(0),
            ..Default::default()
        };

        assert!(tree(&dir, &cli_zero).contains("five"));
    }

    #[test]
    /// Verify correct instantiation of a TreeLevelMeta struct from existing data.
    fn test_tree_level_meta_construct() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let meta = tmpdir.path().metadata().unwrap();

        let options = Options {
            ..Default::default()
        };

        let tree_level_meta = TreeLevelMeta::from(&meta, &options);
        assert_eq!(None, tree_level_meta.user);
        assert_eq!(None, tree_level_meta.group);

        let tree_level_meta = TreeLevelMeta::from(
            &meta,
            &Options {
                user: true,
                ..Default::default()
            },
        );
        assert!(tree_level_meta.user.is_some());
        assert_eq!(None, tree_level_meta.group);

        let tree_level_meta = TreeLevelMeta::from(
            &meta,
            &Options {
                group: true,
                ..Default::default()
            },
        );
        assert_eq!(None, tree_level_meta.user);
        assert!(tree_level_meta.group.is_some());

        let tree_level_meta = TreeLevelMeta::from(
            &meta,
            &Options {
                user: true,
                group: true,
                ..Default::default()
            },
        );
        assert_eq!(None, tree_level_meta.chmods);
        assert!(tree_level_meta.user.is_some());
        assert!(tree_level_meta.group.is_some());

        let tree_level_meta = TreeLevelMeta::from(
            &meta,
            &Options {
                protections: true,
                user: true,
                group: true,
                ..Default::default()
            },
        );
        assert!(tree_level_meta.chmods.is_some());
        assert!(tree_level_meta.user.is_some());
        assert!(tree_level_meta.group.is_some());
    }

    #[test]
    /// Verify that the recusively looking for the longest string works for nested TreeEntry.
    fn test_calc_longest_field() {
        let tree = TreeEntry {
            name: PathBuf::from("first"),
            kind: TreeEntryKind::Directory,
            meta: Default::default(),
            levels: vec![],
            children: TreeChild::Children(vec![
                TreeEntry {
                    name: PathBuf::from("Second"),
                    kind: TreeEntryKind::File,
                    meta: TreeLevelMeta {
                        chmods: None,
                        group: Some("foo".to_string()),
                        user: Some("bar".to_string()),
                    },
                    levels: vec![],
                    children: TreeChild::None,
                },
                TreeEntry {
                    name: PathBuf::from("Third"),
                    kind: TreeEntryKind::Directory,
                    meta: TreeLevelMeta {
                        chmods: None,
                        group: Some("longest".to_string()),
                        user: Some("short".to_string()),
                    },
                    levels: vec![],
                    children: TreeChild::Children(vec![TreeEntry {
                        name: PathBuf::from("Last and Best!"),
                        kind: TreeEntryKind::Directory,
                        meta: TreeLevelMeta {
                            chmods: None,
                            group: Some("short".to_string()),
                            user: Some("not the shortest".to_string()),
                        },
                        levels: vec![],
                        children: TreeChild::Children(vec![TreeEntry {
                            name: PathBuf::from("moar!"),
                            kind: TreeEntryKind::File,
                            meta: Default::default(),
                            levels: vec![],
                            children: TreeChild::None,
                        }]),
                    }]),
                },
            ]),
        };

        let longest_name = tree
            .longest_fieldentry(&|t: &TreeEntry| t.name.as_path().to_string_lossy().to_string());

        let longest_user =
            tree.longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.user));
        let longest_group =
            tree.longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.group));
        assert_eq!(14, longest_name);
        assert_eq!(16, longest_user);
        assert_eq!(7, longest_group);
    }

    #[test]
    // Verify that a String is successfully extracted from an Option or emptry String on None.
    fn test_string_from_opt() {
        struct TestStruct {
            foo: Option<String>,
            bar: Option<String>,
        }

        let test_struct = TestStruct {
            foo: Some("Test".to_string()),
            bar: None,
        };

        assert_eq!("Test", string_from_opt_field(&test_struct.foo));
        assert_eq!("", string_from_opt_field(&test_struct.bar));
    }

    /// Create a directory tree with a directory for which access is restricted.
    fn create_directoy_no_permissions() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        let target_dir = dir.join("root");
        fs::create_dir_all(dir).unwrap();
        fs::create_dir_all(&target_dir).unwrap();

        fs::create_dir_all(target_dir.join("uno")).unwrap();
        File::create(target_dir.join("uno/foo.txt")).unwrap();
        File::create(target_dir.join("uno/bar.txt")).unwrap();

        fs::create_dir_all(target_dir.join("does")).unwrap();
        File::create(target_dir.join("does/baz.md")).unwrap();

        fs::create_dir_all(target_dir.join("tres/ichi/eins/one")).unwrap();
        fs::create_dir_all(target_dir.join("tres/ni/eins/one")).unwrap();
        fs::create_dir_all(target_dir.join("tres/ni/zwei/two")).unwrap();
        fs::create_dir_all(target_dir.join("tres/san/zwei")).unwrap();
        File::create(target_dir.join("tres/ichi/eins/one/hallo.txt")).unwrap();

        fs::set_permissions(target_dir.join("does"), fs::Permissions::from_mode(0o000)).unwrap();
        fs::set_permissions(
            target_dir.join("tres/ichi/eins/one"),
            fs::Permissions::from_mode(0o000),
        )
        .unwrap();
        fs::set_permissions(
            target_dir.join("tres/ni/eins/one"),
            fs::Permissions::from_mode(0o000),
        )
        .unwrap();

        tmpdir
    }

    /// Create a common possible directory tree.
    fn create_directory_tree() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        // let dir = std::path::Path::new("/tmp/rustytree");
        fs::create_dir_all(dir).unwrap();

        let dirs = vec![
            ".vim/session",
            "Desktop",
            "Downloads",
            "Music",
            "My Projects",
            "Pictures/seasons",
            "Pictures/days",
            "Trash/old/obsolete",
        ];

        let files = vec![
            "foo.txt",
            "bar.txt",
            ".vim/vimrc",
            ".vim/.netrwhist",
            ".vim/session/default.vim",
            ".vim/session/testsess.vim",
            ".vim/session/worksess.vim",
            "Downloads/cygwin.exe",
            "Downloads/rustc_1.60.0+dfsg1-1_amd64.deb",
            "Downloads/cargo_0.57.0-7+b1_amd64.deb",
            "Music/one.mp3",
            "Music/two.mp3",
            "Music/three.mp3",
            "Pictures/hello.png",
            "Pictures/days/evening.bmp",
            "Pictures/days/noon.svg",
            "Pictures/days/morning.tiff",
            "Pictures/seasons/autumn.jpg",
            "Pictures/seasons/spring.gif",
            "Pictures/seasons/summer.png",
            "Pictures/seasons/winter.png",
            "Trash/foo.txt",
            "Trash/bar.md",
            "Trash/old/bar.txt",
            "Trash/old/baz.txt",
            "Trash/old/foo.md",
            "Trash/old/obsolete/uno.md",
            "Trash/old/obsolete/does.md",
            "Trash/old/obsolete/tres.txt",
        ];

        for directory in &dirs {
            fs::create_dir_all(dir.join(directory)).unwrap();
        }

        for file in &files {
            File::create(dir.join(file)).unwrap();
        }

        tmpdir
    }

    /// The expected output for the directory tree tests run against without hidden files.
    fn expected_output_standard_nonhidden() -> String {
        let output: String = "
├─bar.txt
├─Desktop
├─Downloads
│   ├─cargo_0.57.0-7+b1_amd64.deb
│   ├─cygwin.exe
│   └─rustc_1.60.0+dfsg1-1_amd64.deb
├─foo.txt
├─Music
│   ├─one.mp3
│   ├─three.mp3
│   └─two.mp3
├─My Projects
├─Pictures
│   ├─days
│   │   ├─evening.bmp
│   │   ├─morning.tiff
│   │   └─noon.svg
│   ├─hello.png
│   └─seasons
│     ├─autumn.jpg
│     ├─spring.gif
│     ├─summer.png
│     └─winter.png
└─Trash
  ├─bar.md
  ├─foo.txt
  └─old
    ├─bar.txt
    ├─baz.txt
    ├─foo.md
    └─obsolete
      ├─does.md
      ├─tres.txt
      └─uno.md"
            .to_string();
        output
    }

    /// The expected output for the directory tree tests run against with hidden files.
    fn expected_output_standard_all() -> String {
        let output: String = "
├─.vim
│   ├─.netrwhist
│   ├─session
│   │   ├─default.vim
│   │   ├─testsess.vim
│   │   └─worksess.vim
│   └─vimrc
├─bar.txt
├─Desktop
├─Downloads
│   ├─cargo_0.57.0-7+b1_amd64.deb
│   ├─cygwin.exe
│   └─rustc_1.60.0+dfsg1-1_amd64.deb
├─foo.txt
├─Music
│   ├─one.mp3
│   ├─three.mp3
│   └─two.mp3
├─My Projects
├─Pictures
│   ├─days
│   │   ├─evening.bmp
│   │   ├─morning.tiff
│   │   └─noon.svg
│   ├─hello.png
│   └─seasons
│     ├─autumn.jpg
│     ├─spring.gif
│     ├─summer.png
│     └─winter.png
└─Trash
  ├─bar.md
  ├─foo.txt
  └─old
    ├─bar.txt
    ├─baz.txt
    ├─foo.md
    └─obsolete
      ├─does.md
      ├─tres.txt
      └─uno.md"
            .to_string();
        output
    }

    /// The expected output for the directory tree tests run against with directories only.
    fn expected_output_standard_dironly_all() -> String {
        let output: String = "
├─.vim
│   └─session
├─Desktop
├─Downloads
├─Music
├─My Projects
├─Pictures
│   ├─days
│   └─seasons
└─Trash
  └─old
    └─obsolete"
            .to_string();
        output
    }
}
