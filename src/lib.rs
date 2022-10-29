//! This crate provides a library to render a graphical representation of a filesystem in a tree
//! like fashion.
//!
//! Each directory is entered in an own thread to increase speed when concurrently traversing the
//! directory tree. Additionally expensive computations can be done like the hash sum generation of
//! files, which will also be done in a separate thread per file.

#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use clap::Parser;
use lscolors::{LsColors, Style};
use rayon::prelude::*;
use std::fs::DirEntry;
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

    /// List of different levels of parent directories up to the root
    levels: Vec<TreeLevel>,

    /// List of child entries to enable a recursive data structure
    children: TreeChild,
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

/// Render the given TreeEntry into a string representation.
fn render_tree_level(entry: &TreeEntry, options: &Options, lscolors: &LsColors) -> String {
    let style = lscolors.style_for_path(&entry.name);
    let ansi_style = style.map(Style::to_ansi_term_style).unwrap_or_default();

    let mut rendered_entry = String::new();
    rendered_entry += "\n";

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

    if let TreeChild::Error(_) = entry.children {
        rendered_entry += " [Cannot access directory]";
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
        children: TreeChild::Children(recurse_paths(path, &indent_level, options).unwrap()),
    };

    let full_representation = render_tree(&entry, options, &lscolors);
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
    lscolors: &LsColors,
) -> TreeRepresentation {
    let mut current_level = render_tree_level(tree_entry, options, lscolors);
    let mut directories: u32 = 0;
    let mut files: u32 = 0;
    if let TreeChild::Children(children) = &tree_entry.children {
        for child in children {
            match child.kind {
                TreeEntryKind::Directory => directories += 1,
                TreeEntryKind::File => files += 1,
            }

            let sub_representation = render_tree(child, options, lscolors);
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
) -> Result<Vec<TreeEntry>, io::Error> {
    let entries = read_dir(path, options)?;
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
            };

            if entry.path().is_dir() && (options.level.is_none() || options.level.unwrap() > indent_level.len()+1) {
                // Either store the successfully retrieved subtree or store an error
                match recurse_paths(&entry.path(), &recurisve_indent, options) {
                    Ok(sub_tree) => tree_entry.children = TreeChild::Children(sub_tree),
                    Err(io_err) => tree_entry.children = TreeChild::Error(io_err.kind()),
                }
            }

            tree_entry
        })
        .collect::<Vec<TreeEntry>>();

    Ok(output)
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
        let out = tree(&dir.path(), &cli);

        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                "\n└─root [Cannot access directory]"
            )
        );
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

        for (level_data, entry_presentation) in test_entries {
            let entry = TreeEntry {
                name: PathBuf::from("filename"),
                kind: TreeEntryKind::File,
                levels: level_data,
                children: TreeChild::None,
            };
            assert_eq!(
                render_tree_level(&entry, &cli, &lscolors),
                entry_presentation
            );
            print!("{}", render_tree_level(&entry, &cli, &lscolors));
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
        };

        let fileentry = TreeEntry {
            name: PathBuf::from("filename"),
            kind: TreeEntryKind::File,
            levels: vec![TreeLevel::TreeBranch],
            children: TreeChild::None,
        };

        assert_eq!(
            render_tree_level(&direntry, &cli_classify, &lscolors),
            "\n├─dirname/"
        );
        assert_eq!(
            render_tree_level(&direntry, &cli_classify_not, &lscolors),
            "\n├─dirname"
        );

        assert_eq!(
            render_tree_level(&fileentry, &cli_classify, &lscolors),
            "\n├─filename"
        );
        assert_eq!(
            render_tree_level(&fileentry, &cli_classify_not, &lscolors),
            "\n├─filename"
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

        fs::set_permissions(target_dir, fs::Permissions::from_mode(0o000)).unwrap();

        tmpdir
    }

    /// Create a common possible directory tree.
    fn create_directory_tree() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        // let dir = std::path::Path::new("/tmp/rustytree");
        fs::create_dir_all(dir).unwrap();

        File::create(dir.join("foo.txt")).unwrap();
        File::create(dir.join("bar.txt")).unwrap();

        fs::create_dir_all(dir.join(".vim/session")).unwrap();
        File::create(dir.join(".vim/vimrc")).unwrap();
        File::create(dir.join(".vim/.netrwhist")).unwrap();
        File::create(dir.join(".vim/session/default.vim")).unwrap();
        File::create(dir.join(".vim/session/testsess.vim")).unwrap();
        File::create(dir.join(".vim/session/worksess.vim")).unwrap();

        fs::create_dir_all(dir.join("Desktop")).unwrap();
        fs::create_dir_all(dir.join("Downloads")).unwrap();
        File::create(dir.join("Downloads/cygwin.exe")).unwrap();
        File::create(dir.join("Downloads/rustc_1.60.0+dfsg1-1_amd64.deb")).unwrap();
        File::create(dir.join("Downloads/cargo_0.57.0-7+b1_amd64.deb")).unwrap();

        fs::create_dir_all(dir.join("Music")).unwrap();
        File::create(dir.join("Music/one.mp3")).unwrap();
        File::create(dir.join("Music/two.mp3")).unwrap();
        File::create(dir.join("Music/three.mp3")).unwrap();

        fs::create_dir_all(dir.join("My Projects")).unwrap();

        fs::create_dir_all(dir.join("Pictures/seasons")).unwrap();
        fs::create_dir_all(dir.join("Pictures/days")).unwrap();
        File::create(dir.join("Pictures/hello.png")).unwrap();
        File::create(dir.join("Pictures/days/evening.bmp")).unwrap();
        File::create(dir.join("Pictures/days/noon.svg")).unwrap();
        File::create(dir.join("Pictures/days/morning.tiff")).unwrap();
        File::create(dir.join("Pictures/seasons/autumn.jpg")).unwrap();
        File::create(dir.join("Pictures/seasons/spring.gif")).unwrap();
        File::create(dir.join("Pictures/seasons/summer.png")).unwrap();
        File::create(dir.join("Pictures/seasons/winter.png")).unwrap();

        fs::create_dir_all(dir.join("Trash/old/obsolete")).unwrap();
        File::create(dir.join("Trash/foo.txt")).unwrap();
        File::create(dir.join("Trash/bar.md")).unwrap();
        File::create(dir.join("Trash/old/bar.txt")).unwrap();
        File::create(dir.join("Trash/old/baz.txt")).unwrap();
        File::create(dir.join("Trash/old/foo.md")).unwrap();
        File::create(dir.join("Trash/old/obsolete/uno.md")).unwrap();
        File::create(dir.join("Trash/old/obsolete/does.md")).unwrap();
        File::create(dir.join("Trash/old/obsolete/tres.txt")).unwrap();

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
