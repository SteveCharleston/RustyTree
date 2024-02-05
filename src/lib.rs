//! This crate provides a library to render a graphical representation of a filesystem in a tree
//! like fashion.
//!
//! Each directory is entered in an own thread to increase speed when concurrently traversing the
//! directory tree. Additionally expensive computations can be done like the hash sum generation of
//! files, which will also be done in a separate thread per file.

#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

mod errors;
mod options;
mod fixtures;

use ansi_term::Color;
use errors::TreeError;
use lscolors::{LsColors, Style};
pub use options::Options;
use options::SignType;
use rayon::prelude::*;
use std::collections::HashSet;
use std::fs::DirEntry;
use std::io::BufRead;
use std::io::Write;
use std::os::unix::prelude::MetadataExt;
use std::path::{Path, PathBuf};
use std::{fs, io};

/// Represent the different possible indentation components of a file.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

    /// Size of file or collectively of all subfiles of a directory
    size: Option<u64>,

    /// Unique Identifier of the file or directory in form of InodeData
    inode: Option<InodeData>,

    /// Type of the content inside the file
    filetype: Option<String>,

    /// External program output used on the file
    exec: Option<Result<String, std::io::ErrorKind>>,
}

impl TreeLevelMeta {
    /// Generate a new TreeLevelMeta from the given data metadata
    fn from(path: &impl AsRef<Path>, options: &Options) -> TreeLevelMeta {
        let meta = match fs::symlink_metadata(path) {
            Ok(m) => m,
            Err(_) => return TreeLevelMeta::default(),
        };
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

        let size = if options.sizes || options.humansize || options.si || options.du {
            Some(meta.len())
        } else {
            None
        };

        let inode = if options.inode {
            Some(InodeData {
                inode: meta.ino(),
                dev: meta.dev(),
            })
        } else {
            None
        };

        let filetype = if options.filetype || options.filtertype.is_some() {
            Some(determine_filetype(path, meta.clone()))
        } else {
            None
        };

        let exec = match &options.exec {
            Some(exec_command) if !meta.is_dir() => {
                let mut cmd = exec_command.clone();
                for argument in cmd.arguments.iter_mut() {
                    if argument.contains("{}") {
                        *argument = argument
                            .replace("{}", path.as_ref().to_string_lossy().to_string().as_str());
                    }
                }
                let cmd_expression = duct::cmd(cmd.command, cmd.arguments);

                let cmd_out = cmd_expression.stderr_to_stdout().unchecked().read();
                Some(cmd_out.map_err(|err| err.kind()))
            }
            _ => None,
        };

        TreeLevelMeta {
            chmods,
            user,
            group,
            size,
            inode,
            filetype,
            exec,
        }
    }
}

/// Make a file uniquely identifiable with the combinaiton of inode and dev
#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
struct InodeData {
    /// Inode number on the filesystem
    inode: u64,
    /// ID of the device that contains the file
    dev: u64,
}

/// Cache the lengths of some TreeEntry fields to avoid recalculating them during drawing.
#[derive(Default)]
struct TreeEntryLengths {
    /// Length of the longest user field
    user: usize,

    /// Length of the longest group field
    group: usize,

    /// Length of the longest size field
    size: usize,

    /// Lenght of the longest filetype field
    filetype: usize,
}

/// Hold the rendered tree as well as number of directories and files to generate the final status
/// line.
#[derive(Clone, Debug)]
struct TreeRepresentation {
    /// Final rendered string representation of the tree
    rendered: String,
    /// Number of directories in the rendered tree
    directories: u32,
    /// Number of files in the rendered tree
    files: u32,
    /// Size of all files combined, if requested
    size: Option<String>,
}

impl TreeRepresentation {
    /// Format information about number of directories, files and possible size as report string.
    fn statistics(&self) -> String {
        format!(
            "{}{} directories, {} files",
            self.size.as_ref().unwrap_or(&"".to_string()),
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
    Error(errors::TreeError),
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
    /// TreeEntry is a Symlink
    Symlink(Box<TreeEntryKind>),
}

/// Send the given text into the given writer.
///
/// Handle common errors that might be encountered when doing I/O. Especially a broken pipe is no
/// reason to abort the program, since such behaviour is very common when combining Unix tools with
/// each other using a pipe.
fn out(text: &str, output: &mut impl Write) {
    // Discard all IO errors, since we do not care for them.
    output.write_all(text.as_bytes()).unwrap_or(())
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
/// * Directory entries could not be read.
fn read_dir(path: &impl AsRef<Path>, options: &Options) -> Result<Vec<DirEntry>, io::Error> {
    let mut paths: Vec<_> = fs::read_dir(path)?
        .collect::<Result<Vec<_>, _>>()? // collect stops upon error and saves that into Result
        .into_par_iter() // if no error, simply loop over the dir contents
        .filter(|r| options.all || !r.file_name().to_string_lossy().starts_with('.')) // hidden
        .filter(|r| !options.dironly || r.path().is_dir()) // directories only
        .filter(|r| {
            r.path().is_dir()
                || options
                    .pattern
                    .as_ref()
                    .map_or(true, |pattern| pattern.is_match(r.file_name()))
        })
        .filter(|r| {
            r.path().is_dir()
                || options
                    .inversepattern
                    .as_ref()
                    .map_or(true, |inverse| !inverse.is_match(r.file_name()))
        })
        .collect();

    paths.sort_by_key(|entry| entry.path().to_string_lossy().to_lowercase());
    Ok(paths)
}

/// Extracts a String from an Option or returns Emptry string if None.
fn string_from_opt_field(field: &Option<String>) -> String {
    field.as_ref().unwrap_or(&"".to_string()).to_string()
}

/// Go through the children of a TreeChild and sum their sizes if existant.
fn sum_children_sizes(children: &TreeChild) -> Option<u64> {
    if let TreeChild::Children(children) = children {
        let sum_children_sizes = children
            .par_iter()
            .map(|child| child.meta.size.unwrap_or(0))
            .sum();

        Some(sum_children_sizes)
    } else {
        Some(0)
    }
}

/// Return the correct character for the given level and font.
fn draw_character(level_type: &TreeLevel, sign_type: &SignType) -> &'static str {
    match sign_type {
        SignType::Ucs => match level_type {
            TreeLevel::Indent => "    ",
            TreeLevel::TreeBar => "│   ",
            TreeLevel::TreeBranch => "├─",
            TreeLevel::TreeFinalBranch => "└─",
        },
        SignType::Ascii => match level_type {
            TreeLevel::Indent => "    ",
            TreeLevel::TreeBar => "|   ",
            TreeLevel::TreeBranch => "|-",
            TreeLevel::TreeFinalBranch => "`-",
        },
    }
}

/// Render an error as a subtree entry to a given entry.
fn render_error(
    error_description: &str,
    error_kind: &TreeError,
    extra_indent: usize,
    levels: &Vec<TreeLevel>,
    options: &Options,
) -> String {
    let mut rendered_error = String::new();
    let error_message = format!(" [{error_description}: {error_kind}]");
    rendered_error += "\n";
    if !options.noindent {
        rendered_error += " ".repeat(extra_indent).as_str();

        // Don't add anything if target dir can't be accessed
        if !&levels.is_empty() {
            // iterate over the levels but omit last entry to draw error children correctly
            for level in &levels[..levels.len().saturating_sub(1)] {
                // We will only ever face Indents or TreeBars here, so no need to transform
                rendered_error += draw_character(level, &options.charset);
            }

            // If parent is last, just add some space to indent onto the next level, otherwise
            // add TREE_SIGN to connect next real entry after the error message
            match levels.last() {
                Some(TreeLevel::TreeFinalBranch) => {
                    rendered_error += draw_character(&TreeLevel::Indent, &options.charset)
                }
                _ => rendered_error += draw_character(&TreeLevel::TreeBar, &options.charset),
            }
        }
        rendered_error += draw_character(&TreeLevel::TreeFinalBranch, &options.charset);
    }

    if options.nocolor {
        rendered_error += error_message.as_str();
    } else {
        rendered_error += Color::Red.paint(error_message).to_string().as_str();
    }

    rendered_error
}

/// Take the size of a file and render a string representation depending on the wanted format.
fn render_size(size: u64, options: &Options) -> String {
    if options.humansize {
        humansize::format_size(size, humansize::BINARY)
    } else if options.si {
        humansize::format_size(size, humansize::DECIMAL)
    } else {
        size.to_string()
    }
}

/// Render the in-tree display of an executed command.
fn render_exec(
    entry: &TreeEntry,
    extra_indent: usize,
    options: &Options,
    output: &mut impl Write,
) -> Result<String, io::Error> {
    if let TreeEntryKind::File = entry.kind {
        // Generate treebranches in front of every line
        let mut rendered_content = String::new();
        rendered_content += "\n";
        rendered_content += calc_sub_entry_indent(entry, extra_indent, options).as_str();

        let cmd_out = if let Some(cmd_out_result) = &entry.meta.exec {
            match cmd_out_result {
                Ok(output) => output.clone(),
                Err(error) => return Err(std::io::Error::from(*error)),
            }
        } else {
            "".to_string()
        };

        for line in cmd_out.lines() {
            if !line.is_empty() {
                // remove control characters and avoid lines that would otherwise be empty or
                // malformatted output from e.g. a line feed
                let line = line.replace(|c: char| c.is_control(), "");

                // if line was not empty before but now, we should skip it completly
                if line.is_empty() {
                    continue;
                }
            }

            out(&rendered_content, output);
            out(line, output);
        }
    }
    Ok("".to_string()) // replace with output later on
}

/// Read the contents of a file and return them.
///
/// File contents that can be displayed will be returned, whereas non-printable lines will be
/// skipped. Retain newlines that where originally in the file while skipping newlines that where
/// introduced while filtering the non-printable characters.
fn cat_file(
    entry: &TreeEntry,
    extra_indent: usize,
    options: &Options,
    output: &mut impl Write,
) -> Result<(), io::Error> {
    if let TreeEntryKind::File = entry.kind {
        let file = fs::File::open(&entry.name)?;
        let reader = io::BufReader::new(file);

        // Generate treebranches in front of every line
        let mut rendered_content = String::new();
        rendered_content += "\n";
        rendered_content += calc_sub_entry_indent(entry, extra_indent, options).as_str();

        for line in reader.lines() {
            let line = match line {
                Ok(text) => text,
                Err(_) => continue,
            };

            if !line.is_empty() {
                // remove control characters and avoid lines that would otherwise be empty or
                // malformatted output from e.g. a line feed
                let line = line.replace(|c: char| c.is_control(), "");

                // if line was not empty before but now, we should skip it completly
                if line.is_empty() {
                    continue;
                }
            }

            out(&rendered_content, output);
            out(&line, output);
        }
    }
    Ok(())
}

/// Calculate the indent for content that is shown beneath an entry.
fn calc_sub_entry_indent(entry: &TreeEntry, extra_indent: usize, options: &Options) -> String {
    let mut rendered_content = String::new();
    if !options.noindent {
        rendered_content += " ".repeat(extra_indent).as_str();
        for level in &entry.levels {
            rendered_content += match level {
                TreeLevel::Indent => draw_character(level, &options.charset),
                TreeLevel::TreeBar => draw_character(level, &options.charset),
                TreeLevel::TreeFinalBranch => draw_character(&TreeLevel::Indent, &options.charset),
                TreeLevel::TreeBranch => draw_character(&TreeLevel::TreeBar, &options.charset),
            }
        }
    } else {
        rendered_content += draw_character(&TreeLevel::Indent, &options.charset);
    }

    rendered_content
}

/// Calculate the filetype of a given file
fn determine_filetype(path: &impl AsRef<Path>, meta: fs::Metadata) -> String {
    if meta.is_symlink() {
        // Symlinks are not recognized so do it here
        "inode/symlink".to_string()
    } else {
        // According to RFC-2046, unknown data is `application/octet-stream`
        let generic_mime_type = "application/octet-stream";

        // start with the most generic content type `all/all`
        tree_magic::TYPE.hash.get("all/all").map_or_else(
            || generic_mime_type.to_string(), // return generic if MIME DB doesn't exist
            |node| {
                tree_magic::from_filepath_node(*node, path.as_ref())
                    .unwrap_or(generic_mime_type.to_string())
            },
        )
    }
}

/// Render the given TreeEntry into a string representation.
fn render_tree_level(
    entry: &TreeEntry,
    options: &Options,
    sizes: &TreeEntryLengths,
    lscolors: &LsColors,
    output: &mut impl Write,
) -> String {
    let style = lscolors.style_for_path(&entry.name);
    let ansi_style = style.map(Style::to_ansi_term_style).unwrap_or_default();

    let mut rendered_entry = String::new();
    if !entry.levels.is_empty() {
        // no newline in front of root directory
        rendered_entry += "\n";
    }

    if options.filetype {
        // Metadata might also be set with filtertype
        if let Some(filetype) = &entry.meta.filetype {
            rendered_entry += format!("{:width$}", filetype, width = sizes.filetype + 1).as_str();
        }
    }
    if let Some(inode_data) = &entry.meta.inode {
        rendered_entry += format!("{} ", inode_data.inode).as_str();
    }
    if let Some(chmods) = &entry.meta.chmods {
        rendered_entry += format!("{} ", unix_mode::to_string(*chmods).as_str()).as_str();
    }

    if let Some(user) = &entry.meta.user {
        rendered_entry += format!("{:width$}", user, width = sizes.user + 1).as_str();
    }

    if let Some(group) = &entry.meta.group {
        rendered_entry += format!("{:width$}", group, width = sizes.group + 1).as_str();
    }

    if let Some(size) = &entry.meta.size {
        rendered_entry += format!(
            "{:<width$}",
            render_size(*size, options),
            width = sizes.size + 1
        )
        .as_str();
    }

    let extra_indent = rendered_entry.len().saturating_sub(1); // save indent to format errors, subtract newline

    if !options.noindent {
        for level in &entry.levels {
            rendered_entry += draw_character(level, &options.charset);
        }
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

    if let TreeEntryKind::Symlink(_) = entry.kind {
        let link_style = match entry.name.metadata() {
            Ok(metadata) => lscolors.style_for_path_with_metadata(&entry.name, Some(&metadata)),
            Err(_) => lscolors.style_for_indicator(lscolors::Indicator::MissingFile),
        };

        let link_ansi_style = link_style
            .map(Style::to_ansi_term_style)
            .unwrap_or_default();

        let link_target = link_ansi_style
            .paint(entry.name.read_link().unwrap_or_default().to_string_lossy())
            .to_string();

        rendered_entry += " ➜ ";
        rendered_entry += link_target.as_str();
    }

    if let TreeChild::Error(error_kind) = &entry.children {
        // if we can't access a directory, show a childentry with an error message
        rendered_entry += render_error(
            "Cannot access directory",
            error_kind,
            extra_indent,
            &entry.levels,
            options,
        )
        .as_str();
    }

    out(&rendered_entry, output);

    if options.exec.is_some() {
        if let TreeEntryKind::File = entry.kind {
            match render_exec(entry, extra_indent, options, output) {
                Ok(_) => (),
                Err(error) => out(
                    &render_error(
                        "Cannot execute command",
                        &TreeError::IoError(error.kind()),
                        extra_indent,
                        &entry.levels,
                        options,
                    ),
                    output,
                ),
            };
        }
    }

    if options.cat {
        if let TreeEntryKind::File = entry.kind {
            match cat_file(entry, extra_indent, options, output) {
                Ok(_) => (),
                Err(error) => out(
                    &render_error(
                        "Cannot access file",
                        &TreeError::IoError(error.kind()),
                        extra_indent,
                        &entry.levels,
                        options,
                    ),
                    output,
                ),
            }
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
    let mut out: Vec<u8> = Vec::new();
    tree_writer(path, options, &mut out);
    String::from_utf8_lossy(&out).to_string()
}

/// Generate a tree representation of the filesystem and write that into a Writer.
///
/// Walk the filesystem starting from the given directory and visit all child directories and files
/// recursively. Render all the files into a tree like string representation. Each directory visit
/// is done in a thread and also some expensive computations might be executed which will also be
/// threaded to distribute the load amongst the available cores. Directly write the tree
/// representation into the given writer, which will reduce memory usage in case that the writer is
/// stdout.
pub fn tree_writer(path: &impl AsRef<Path>, options: &Options, output: &mut impl Write) {
    let indent_level: Vec<TreeLevel> = Vec::new();
    let lscolors = match options.nocolor {
        false => LsColors::from_env().unwrap_or_default(),
        true => LsColors::empty(),
    };

    let seen: Option<HashSet<InodeData>> = if options.followlinks {
        Some(HashSet::new())
    } else {
        None
    };

    let mut entry = TreeEntry {
        name: path.as_ref().to_path_buf(),
        kind: TreeEntryKind::Directory,
        levels: indent_level.clone(),
        children: recurse_paths(path, &indent_level, options, &seen),
        meta: TreeLevelMeta::from(&path, options),
    };

    entry.children = recalc_tree_levels(&entry, &indent_level);

    if options.du {
        entry.meta.size = sum_children_sizes(&entry.children);
    }

    // format differently depending on if we display the size human readable
    let size_func = if options.humansize {
        |t: &TreeEntry| humansize::format_size(t.meta.size.unwrap_or(0), humansize::BINARY)
    } else if options.si {
        |t: &TreeEntry| humansize::format_size(t.meta.size.unwrap_or(0), humansize::DECIMAL)
    } else {
        |t: &TreeEntry| t.meta.size.unwrap_or(0).to_string()
    };

    let sizes = TreeEntryLengths {
        user: entry.longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.user)),
        group: entry.longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.group)),
        size: entry.longest_fieldentry(&size_func),
        filetype: entry
            .longest_fieldentry(&|t: &TreeEntry| string_from_opt_field(&t.meta.filetype)),
    };

    let full_representation = render_tree(&entry, options, &sizes, &lscolors, output);

    if !options.noreport {
        out(&format!("\n\n{}", full_representation.statistics()), output);
    }
}

/// Levels of a TreeEntry might get messed up due to filtering, so calculate them correctly here.
fn recalc_tree_levels(tree_entry: &TreeEntry, indent_level: &[TreeLevel]) -> TreeChild {
    let new_children = if let TreeChild::Children(children) = &tree_entry.children {
        let fixed_children = children
            .iter()
            .enumerate()
            .map(|(i, entry)| {
                let mut new_entry = entry.clone();
                let mut current_indent: Vec<TreeLevel> = indent_level.to_vec();
                let mut recurisve_indent: Vec<TreeLevel> = indent_level.to_vec();
                let is_last = i == children.len() - 1;
                if is_last {
                    current_indent.push(TreeLevel::TreeFinalBranch);
                    recurisve_indent.push(TreeLevel::Indent);
                } else {
                    current_indent.push(TreeLevel::TreeBranch);
                    recurisve_indent.push(TreeLevel::TreeBar);
                };
                new_entry.children = recalc_tree_levels(&new_entry, &recurisve_indent);
                new_entry.levels = current_indent;
                new_entry
            })
            .collect::<Vec<TreeEntry>>();
        TreeChild::Children(fixed_children)
    } else {
        tree_entry.children.clone()
    };

    new_children
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
    output: &mut impl Write,
) -> TreeRepresentation {
    let mut current_level = render_tree_level(tree_entry, options, sizes, lscolors, output);
    let mut directories: u32 = 0;
    let mut files: u32 = 0;
    if let TreeChild::Children(children) = &tree_entry.children {
        for child in children {
            match child.kind {
                TreeEntryKind::Directory => directories += 1,
                TreeEntryKind::File => files += 1,
                TreeEntryKind::Symlink(_) => {
                    if child.name.is_dir() {
                        directories += 1;
                    } else {
                        files += 1;
                    }
                }
            }

            let sub_representation = render_tree(child, options, sizes, lscolors, output);
            current_level += sub_representation.rendered.as_str();
            directories += sub_representation.directories;
            files += sub_representation.files;
        }
    }

    // Render summed size if requested
    let size_representation = if options.du {
        let unit_suffix = if !options.si && !options.humansize {
            " B"
        } else {
            ""
        };

        tree_entry
            .meta
            .size
            .map(|size| format!("{}{} used in ", render_size(size, options), unit_suffix))
    } else {
        None
    };

    TreeRepresentation {
        rendered: current_level,
        directories,
        files,
        size: size_representation,
    }
}

/// Actually do the work of computing the tree.
fn recurse_paths(
    path: &impl AsRef<Path>,
    indent_level: &[TreeLevel],
    options: &Options,
    seen: &Option<HashSet<InodeData>>,
) -> TreeChild {
    let entries = match read_dir(path, options) {
        Ok(entries) => entries,
        Err(io_err) => return TreeChild::Error(TreeError::IoError(io_err.kind())),
    };

    let output = entries
        .into_par_iter()
        .filter_map(|entry| {
            let mut recurisve_indent: Vec<TreeLevel> = indent_level.to_vec();
            // will be fixed later, so basically just count the depth here
            recurisve_indent.push(TreeLevel::Indent);

            let mut tree_entry = TreeEntry {
                name: entry.path(),
                kind: if entry.path().is_symlink() {
                    if entry.path().is_dir() {
                        TreeEntryKind::Symlink(Box::new(TreeEntryKind::Directory))
                    } else {
                        TreeEntryKind::Symlink(Box::new(TreeEntryKind::File))
                    }
                } else if entry.path().is_dir() {
                    TreeEntryKind::Directory
                } else {
                    TreeEntryKind::File
                },
                levels: recurisve_indent.to_vec(),
                children: TreeChild::None,
                meta: TreeLevelMeta::from(&entry.path(), options),
            };

            if entry.path().is_dir()
                && (!entry.path().is_symlink() || options.followlinks)
                && (options.level.is_none()
                    || options.level.unwrap() == 0
                    || options.level.unwrap() > indent_level.len() + 1)
            {
                let new_seen = match seen {
                    Some(old_seen) => {
                        let inode_data = InodeData {
                            inode: fs::metadata(entry.path()).unwrap().ino(),
                            dev: fs::metadata(entry.path()).unwrap().dev(),
                        };

                        let mut new_seen = old_seen.clone();
                        new_seen.insert(inode_data);
                        Some(new_seen)
                    }
                    None => None,
                };

                // If we don't look out for duplicate inodes or a new inode has been added, we are
                // not in a symlink loop. If a new inode has not been added, then it was already
                // seen and so we are in a loop.
                if new_seen.is_some() && &new_seen == seen {
                    tree_entry.children = TreeChild::Error(TreeError::SymlinkLoop)
                } else if options.xdev
                    && entry.metadata().unwrap().dev() != options.path.metadata().unwrap().dev()
                {
                    tree_entry.children = TreeChild::Error(TreeError::FilesystemBoundary)
                } else {
                    // Either store the successfully retrieved subtree or store an error
                    tree_entry.children =
                        recurse_paths(&entry.path(), &recurisve_indent, options, &new_seen);

                    if options.prune {
                        if let TreeChild::Children(children_list) = &tree_entry.children {
                            if children_list.is_empty() {
                                return None; //filter empty directories
                            }
                        }
                    }

                    if options.du {
                        tree_entry.meta.size = sum_children_sizes(&tree_entry.children)
                    }
                }
            } else if let Some(filetypepattern) = &options.filtertype {
                if let Some(filetype) = &tree_entry.meta.filetype {
                    if !filetypepattern.is_match(filetype) {
                        return None; // filter the entry
                    }
                }
            }

            Some(tree_entry)
        })
        .collect::<Vec<TreeEntry>>();

    TreeChild::Children(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use options::{parse_pattern, ExecCommand};
    use pretty_assertions::assert_eq;
    use std::fs;
    use std::fs::File;
    use std::os::unix::prelude::PermissionsExt;
    use tempfile;
    use fixtures::*;

    #[test]
    /// Verify that a generated filesystem tree is as expected with hidden files.
    fn test_print_paths_all() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_path_buf(),
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
            path: dir.path().to_path_buf(),
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
            path: dir.path().to_path_buf(),
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
    /// Verify that sizes are computed correctly
    fn test_sizes() {
        let dir = create_directory_tree();
        let cli_nonhuman = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            sizes: true,
            ..Default::default()
        };

        let cli_human = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            sizes: true,
            humansize: true,
            ..Default::default()
        };

        let cli_sumsize = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            sizes: true,
            du: true,
            ..Default::default()
        };

        let out_nonhuman = tree(&dir.path(), &cli_nonhuman);
        let out_human = tree(&dir.path(), &cli_human);
        let out_sumsize = tree(&dir.path(), &cli_sumsize);

        println!("{}", out_nonhuman);
        assert_eq!(
            out_nonhuman,
            format!(
                "4096 {}{}",
                dir.path().to_str().unwrap(),
                expected_output_standard_sizes()
            )
        );

        println!("{}", out_human);
        assert_eq!(
            out_human,
            format!(
                "4 KiB {}{}",
                dir.path().to_str().unwrap(),
                expected_output_standard_sizes_human()
            )
        );

        // Just check if the sum of all subdirs is calculated correctly
        println!("{}", out_sumsize);
        assert!(out_sumsize.starts_with("504"));
    }

    #[test]
    // Verify that a report for sum of all sizes contains the used space.
    fn test_sum_size_report() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");

        let cli_du_only = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            du: true,
            ..Default::default()
        };

        let cli_human = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            du: true,
            humansize: true,
            ..Default::default()
        };

        let out_du_only = tree(&tmpdir.path(), &cli_du_only);
        let out_human = tree(&tmpdir.path(), &cli_human);
        println!("{}", out_human);
        println!("{}", out_du_only);
        assert!(out_du_only
            .lines()
            .last()
            .unwrap()
            .starts_with("0 B used in"));
        assert!(out_human.lines().last().unwrap().starts_with("0 B used in"));
    }

    #[test]
    /// Verify that a non Children TreeChild is handled as Zero.
    fn test_sum_children_invalid() {
        let children = TreeChild::None;
        assert_eq!(Some(0), sum_children_sizes(&children));
    }

    #[test]
    /// Verify that SI sizes and humansize are rendered differently.
    fn test_differentiate_size_si_human() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");

        let cli_human = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            sizes: true,
            humansize: true,
            ..Default::default()
        };

        let cli_si = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            sizes: true,
            si: true,
            ..Default::default()
        };

        let out_human = tree(&tmpdir.path(), &cli_human);
        let out_si = tree(&tmpdir.path(), &cli_si);
        println!("{}", out_human);
        println!("{}", out_si);
        assert!(out_human.starts_with("4 KiB"));
        assert!(out_si.starts_with("4.10 kB"));
    }

    #[test]
    /// Verify that filetypes are recognized
    fn test_filetypes() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            filetype: true,
            ..Default::default()
        };

        let out = tree(&dir.path(), &cli);

        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "inode/directory {}{}",
                dir.path().to_str().unwrap(),
                expected_output_filetypes(),
            )
        );
    }

    #[test]
    /// Verify that filetypes are recognized
    fn test_filetypes_empty_symlink() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        std::os::unix::fs::symlink("target", tmpdir.path().join("dangling")).unwrap();

        let cli = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            filetype: true,
            noreport: true,
            ..Default::default()
        };

        let out = tree(&tmpdir.path(), &cli);
        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "inode/directory {}\n{}",
                tmpdir.path().to_str().unwrap(),
                "inode/symlink   └─dangling ➜ target"
            )
        );
    }

    #[test]
    fn test_prune_empty_dirs() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            prune: true,
            ..Default::default()
        };

        let out = tree(&dir.path(), &cli);
        println!("{}", out);
        let expected_tree = "
├─bar.txt
├─Downloads
│   ├─cargo_0.57.0-7+b1_amd64.deb
│   ├─cygwin.exe
│   └─rustc_1.60.0+dfsg1-1_amd64.deb
├─foo.txt
├─Music
│   ├─one.mp3
│   ├─three.mp3
│   └─two.mp3
├─Pictures
│   ├─days
│   │   ├─evening.bmp
│   │   ├─morning.tiff
│   │   └─noon.svg
│   ├─hello.png
│   └─seasons
│       ├─autumn.jpg
│       ├─spring.gif
│       ├─summer.png
│       └─winter.png
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
            └─uno.md";
        assert_eq!(
            out,
            format!("{}{}", dir.path().to_str().unwrap(), expected_tree)
        );
    }

    #[test]
    fn test_filetypefilter() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let unfiltered_output = "
inode/directory           ├─bar
application/x-shellscript │   ├─one.rb
text/x-python3            │   ├─three.py
application/x-shellscript │   └─two.rb
inode/directory           ├─baz
application/x-shellscript │   └─three.rb
inode/directory           └─foo
text/x-python3                ├─one.py
text/x-python3                └─two.py";

        let cli = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            filetype: true,
            noreport: true,
            filtertype: parse_pattern("*").ok(),
            ..Default::default()
        };

        let cli_shell = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            filetype: true,
            noreport: true,
            filtertype: parse_pattern("*shell*").ok(),
            ..Default::default()
        };

        let cli_python = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            filetype: true,
            noreport: true,
            filtertype: parse_pattern("*python*").ok(),
            ..Default::default()
        };

        let dir = tmpdir.path();
        fs::create_dir_all(dir.join("foo")).unwrap();
        fs::create_dir_all(dir.join("bar")).unwrap();
        fs::create_dir_all(dir.join("baz")).unwrap();

        for pyfile in vec!["foo/one.py", "foo/two.py", "bar/three.py"] {
            File::create(dir.join(pyfile)).unwrap();
            fs::write(dir.join(pyfile), "#!/usr/bin/python3").unwrap();
        }

        for shfile in vec!["bar/one.rb", "bar/two.rb", "baz/three.rb"] {
            File::create(dir.join(shfile)).unwrap();
            fs::write(dir.join(shfile), "#!/bin/sh\n#foo bar\n#testcomment\n").unwrap();
        }

        let out = tree(&tmpdir.path(), &cli);
        let out_shell = tree(&tmpdir.path(), &cli_shell);
        let out_python = tree(&tmpdir.path(), &cli_python);
        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "inode/directory           {}{}",
                tmpdir.path().to_str().unwrap(),
                unfiltered_output,
            )
        );

        assert_eq!(out_shell.matches("shellscript").count(), 3);
        assert_eq!(out_shell.matches("python").count(), 0);

        assert_eq!(out_python.matches("shellscript").count(), 0);
        assert_eq!(out_python.matches("python").count(), 3);
    }

    #[test]
    /// Verify that the text content of files it outputted correctly.
    ///
    /// Also check that nonprintable characters are filtered out and as a consequence that
    /// completly binary files do not produce any output at all.
    fn test_cat_text() {
        let dir = create_directory_tree_texts();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            cat: true,
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);
        print!("{out}");
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_output_directory_tree_text()
            )
        );
    }

    #[test]
    /// Verify that an error message is rendered if cat is tried on an unaccessible file.
    fn test_cat_file_error() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::write(dir.join("testfile"), "").unwrap();
        fs::set_permissions(dir.join("testfile"), fs::Permissions::from_mode(0o000)).unwrap();
        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            cat: true,
            ..Default::default()
        };
        let out = tree(&dir, &cli);
        assert!(out.ends_with("└─testfile\n    └─ [Cannot access file: permission denied]"))
    }

    #[test]
    /// Verify that an invalid utf-8 sequence is simply skipped.
    fn test_cat_file_invalid_utf8() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::write(dir.join("testfile"), b"line 1\nline 2\xF1\x80\x80\nline 3").unwrap();
        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            cat: true,
            ..Default::default()
        };
        let out = tree(&dir, &cli);
        print!("{out}");
        assert!(out.ends_with("└─testfile\n    line 1\n    line 3"))
    }

    #[test]
    /// Verify that an external program tail -3 is executed correct with the filename as argument.
    fn test_exec_tail_three() {
        let dir = create_directory_tree_texts();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("tail -3 {}").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);
        print!("{out}");
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_output_directory_tree_tail_three()
            )
        );
    }

    #[test]
    /// Verify that an external program is executed correct when the filename contains a space.
    fn test_exec_filename_spaces() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::write(dir.join("test file"), "line 1\nline 2\nline 3\nline 4").unwrap();
        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("tail -3 {}").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir, &cli);
        print!("{out}");
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.to_str().unwrap(),
                "\n└─test file\n    line 2\n    line 3\n    line 4",
            )
        );
    }

    #[test]
    /// Verify that external program output is correctly collected when it returns with error.
    fn test_exec_errcode() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::write(dir.join("test_file"), "line 1\nline 2\nline 3\nline 4").unwrap();
        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("false").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir, &cli);
        print!("{out}");
        assert_eq!(
            out,
            format!("{}{}", dir.to_str().unwrap(), "\n└─test_file",)
        );
    }

    #[test]
    /// Verify that a missing or unexecutable cmd generates an error message.
    fn test_exec_missing_cmd() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();

        fs::write(dir.join("testfile"), "").unwrap();

        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("nonexisting {}").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir, &cli);
        print!("{out}");
        assert!(out.ends_with("    └─ [Cannot execute command: entity not found]"));
    }

    #[test]
    /// Verify that a the filename is replaced multiple times.
    fn test_exec_multiple_filename() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        let full_filename_path = format!("{}/{}", dir.as_os_str().to_string_lossy(), "testfile");

        fs::write(dir.join("testfile"), "").unwrap();

        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("echo {} -- {}").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir, &cli);
        print!("{out}");
        assert!(out.ends_with(format!("{} -- {}", full_filename_path, full_filename_path).as_str()));
    }

    #[test]
    /// Verify that filename in quotes is properly replaced.
    ///
    /// Since we use shell_words to split the arguments like they would be split in a shell, it
    /// might happen that filenames are contained in a longer argument with more text. It must be
    /// ensured that filenames are replaced in all cases, even when surrounded by more text.
    fn test_exec_multiple_filename_in_quotes() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        let full_filename_path = format!("{}/{}", dir.as_os_str().to_string_lossy(), "testfile");

        fs::write(dir.join("testfile"), "").unwrap();

        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("echo '{} -- {}'").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir, &cli);
        print!("{out}");
        assert!(out.ends_with(format!("{} -- {}", full_filename_path, full_filename_path).as_str()));
    }

    #[test]
    /// Verify that a missing exec option leads to immediate return of exec_file.
    fn test_exec_no_cmd() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();

        let entry = TreeEntry {
            name: dir.to_path_buf(),
            kind: TreeEntryKind::File,
            meta: TreeLevelMeta::default(),
            levels: vec![],
            children: TreeChild::None,
        };
        let mut out: Vec<u8> = Vec::new();
        fs::write(dir.join("testfile"), "").unwrap();

        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: None,
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let output = render_exec(&entry, 0, &cli, &mut out).unwrap();

        print!("{output}");
        assert_eq!("", output);
    }

    #[test]
    /// Verify that stderr and stdout are both visible in the output.
    fn test_exec_stderr() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();

        fs::write(dir.join("testfile"), "").unwrap();

        let cli = Options {
            path: dir.to_path_buf(),
            nocolor: true,
            noreport: true,
            exec: Some(ExecCommand::from("perl -e 'STDOUT->autoflush(1); say STDOUT stdout; say STDERR stderr; say STDOUT stdout'").unwrap()),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir, &cli);
        print!("{out}");
        assert!(out.ends_with(
            "
└─testfile
    stdout
    stderr
    stdout"
        ));
    }

    #[test]
    // Verify that for the noindent option only one indent is returned for sub entries.
    fn test_calc_sub_entry_indent_noindent() {
        let entry = TreeEntry {
            name: PathBuf::from("foofile"),
            kind: TreeEntryKind::File,
            meta: TreeLevelMeta::default(),
            levels: vec![],
            children: TreeChild::None,
        };

        let cli = Options {
            path: PathBuf::from("foodir"),
            nocolor: true,
            noreport: true,
            noindent: true,
            ..Default::default()
        };
        let output = calc_sub_entry_indent(&entry, 0, &cli);

        print!("{output}");
        assert_eq!(draw_character(&TreeLevel::Indent, &SignType::Ucs), output);
    }

    #[test]
    /// Verify that symlinks are displayed or followed as requested.
    fn test_handle_symlinks() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        std::os::unix::fs::symlink(tmpdir.path(), tmpdir.path().join("foo")).unwrap();
        std::os::unix::fs::symlink("target", tmpdir.path().join("dangling")).unwrap();

        let cli = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            ..Default::default()
        };

        let out = tree(&tmpdir.path(), &cli);
        println!("{}", out);
        assert!(out.contains("dangling ➜ target"));
        assert!(out.contains("1 directories, 1 files")); // files and dires are counted correctly
    }

    #[test]
    /// Verify that symlink loops are detected.
    fn test_handle_looping_symlinks() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::create_dir_all(dir.join("foo/bar/baz")).unwrap();
        std::os::unix::fs::symlink(dir.join("foo"), dir.join("foo/bar/baz/bang")).unwrap();

        let cli = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            followlinks: true,
            ..Default::default()
        };

        let out = tree(&tmpdir.path(), &cli);
        println!("{}", out);
        assert!(out.contains("Cannot access directory: Symlink loop detected"));
    }

    #[test]
    /// Verify that filesystem boundaries are respected.
    fn test_handle_fs_boundary_xdev() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::create_dir_all(dir.join("foo/bar/baz")).unwrap();
        fs::create_dir_all(dir.join("/dev/shm/testdir")).unwrap();
        std::os::unix::fs::symlink(Path::new("/dev/shm/"), dir.join("foo/bar/baz/bang")).unwrap();

        let cli = Options {
            path: tmpdir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            followlinks: true,
            xdev: true,
            ..Default::default()
        };

        let out = tree(&tmpdir.path(), &cli);
        println!("{}", out);
        assert!(out.contains("Cannot access directory: Not crossing filesystem boundary"));
    }

    #[test]
    /// Verify that legal and illegal pattern are parsed and a correct GlobSet is created.
    fn test_parse_pattern() {
        let out = parse_pattern("foo*|bar*").unwrap();
        assert_eq!(out.len(), 2);
        assert_eq!(out.matches("foo"), vec![0]);
        assert_eq!(out.matches("bar"), vec![1]);

        let out_err = parse_pattern("[*");
        assert!(out_err.is_err());
    }

    #[test]
    /// Verify that filtering of files with globpattern works.
    fn test_filter_pattern() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            pattern: parse_pattern("*.md|*.txt").ok(),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);

        let expected_tree = "
├─bar.txt
├─Desktop
├─Downloads
├─foo.txt
├─Music
├─My Projects
├─Pictures
│   ├─days
│   └─seasons
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

        assert_eq!(
            out,
            format!("{}{}", dir.path().to_str().unwrap(), expected_tree)
        );
    }

    #[test]
    /// Verify that inverse filtering of files with globpattern works.
    fn test_filter_inverse_pattern() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            inversepattern: parse_pattern("*.md|*.txt").ok(),
            ..Default::default()
        };
        println!("tmpdir: {:?}", dir);
        let out = tree(&dir.path(), &cli);

        let expected_tree = "
├─Desktop
├─Downloads
│   ├─cargo_0.57.0-7+b1_amd64.deb
│   ├─cygwin.exe
│   └─rustc_1.60.0+dfsg1-1_amd64.deb
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
│       ├─autumn.jpg
│       ├─spring.gif
│       ├─summer.png
│       └─winter.png
└─Trash
    └─old
        └─obsolete"
            .to_string();

        assert_eq!(
            out,
            format!("{}{}", dir.path().to_str().unwrap(), expected_tree)
        );
    }

    #[test]
    /// Verify that file tree is rendered correctly when option to stay on FS is given.
    fn test_xdev_works() {
        let dir = create_directory_tree();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            xdev: true,
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
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            ..Default::default()
        };

        let cli_colored = Options {
            path: dir.path().to_path_buf(),
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
    │   │       └─one
    │   │           └─ [Cannot access directory: permission denied]
    │   ├─ni
    │   │   ├─eins
    │   │   │   └─one
    │   │   │       └─ [Cannot access directory: permission denied]
    │   │   └─zwei
    │   │       └─two
    │   └─san
    │       └─zwei
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
    /// Verify that missing permissions are indented correct when combined with other metadata.
    fn test_no_read_permissions_formatting() {
        let dir = create_directoy_no_permissions();
        let cli = Options {
            path: dir.path().to_path_buf(),
            nocolor: true,
            noreport: true,
            protections: true,
            ..Default::default()
        };

        let expected_tree = "
drwxrwxr-x └─root
d---------     ├─does
               │   └─ [Cannot access directory: permission denied]
drwxrwxr-x     ├─tres
drwxrwxr-x     │   ├─ichi
drwxrwxr-x     │   │   └─eins
d---------     │   │       └─one
               │   │           └─ [Cannot access directory: permission denied]
drwxrwxr-x     │   ├─ni
drwxrwxr-x     │   │   ├─eins
d---------     │   │   │   └─one
               │   │   │       └─ [Cannot access directory: permission denied]
drwxrwxr-x     │   │   └─zwei
drwxrwxr-x     │   │       └─two
drwxrwxr-x     │   └─san
drwxrwxr-x     │       └─zwei
drwxrwxr-x     └─uno
-rw-rw-r--         ├─bar.txt
-rw-rw-r--         └─foo.txt";

        let out = tree(&dir.path(), &cli);
        println!("{}", out);
        assert_eq!(
            out,
            format!(
                "drwxrwxr-x {}{}",
                dir.path().to_str().unwrap(),
                expected_tree
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
                "\n    └─filename",
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
            path: PathBuf::from("."),
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
                render_tree_level_to_string(&entry, &cli, &lengths, &lscolors),
                entry_presentation
            );
            print!(
                "{}",
                render_tree_level_to_string(&entry, &cli, &lengths, &lscolors)
            );
        }
    }

    #[test]
    /// Verify that directories are correctly classified with a trailing slash.
    fn test_render_tree_entry_classify() {
        let cli_classify = Options {
            path: PathBuf::from("."),
            classify: true,
            nocolor: true,
            ..Default::default()
        };

        let lscolors = LsColors::default();
        let lengths = TreeEntryLengths::default();

        let cli_classify_not = Options {
            path: PathBuf::from("."),
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
            render_tree_level_to_string(&direntry, &cli_classify, &lengths, &lscolors),
            "\n├─dirname/"
        );
        assert_eq!(
            render_tree_level_to_string(&direntry, &cli_classify_not, &lengths, &lscolors),
            "\n├─dirname"
        );

        assert_eq!(
            render_tree_level_to_string(&fileentry, &cli_classify, &lengths, &lscolors),
            "\n├─filename"
        );
        assert_eq!(
            render_tree_level_to_string(&fileentry, &cli_classify_not, &lengths, &lscolors),
            "\n├─filename"
        );
    }

    #[test]
    /// Verify that lengths are taken from the lengths argument and that all options
    /// are displayed appropriately.
    fn test_render_tree_all_options() {
        let cli_group_user = Options {
            path: PathBuf::from("."),
            nocolor: true,
            protections: true,
            user: true,
            group: true,
            inode: true,
            ..Default::default()
        };

        let lscolors = LsColors::default();
        let lengths_too_small = TreeEntryLengths {
            user: 0,
            group: 0,
            size: 0,
            filetype: 0,
        };
        let lengths_correct = TreeEntryLengths {
            user: 7,
            group: 8,
            size: 4,
            filetype: 0,
        };
        let lengths_too_big = TreeEntryLengths {
            user: 10,
            group: 20,
            size: 15,
            filetype: 0,
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
                size: Some(1337),
                inode: Some(InodeData {
                    inode: 42002469,
                    dev: 12345678,
                }),
                filetype: None,
                exec: None,
            },
        };
        assert_eq!(
            render_tree_level_to_string(&fileentry, &cli_group_user, &lengths_too_small, &lscolors),
            "\n42002469 -rw-r--r-- foouserfoogroup1337├─filename"
        );
        assert_eq!(
            render_tree_level_to_string(&fileentry, &cli_group_user, &lengths_correct, &lscolors),
            "\n42002469 -rw-r--r-- foouser foogroup 1337 ├─filename"
        );
        assert_eq!(
            render_tree_level_to_string(&fileentry, &cli_group_user, &lengths_too_big, &lscolors),
            "\n42002469 -rw-r--r-- foouser    foogroup             1337            ├─filename"
        );
    }

    #[test]
    /// Verify that the nocolor option turns colorization off.
    fn test_render_tree_nocolor() {
        std::env::set_var("LS_COLORS", ""); // use default colors for testability
        let dir = create_directory_tree();

        let cli_colorful = Options {
            path: PathBuf::from("."),
            nocolor: false,
            ..Default::default()
        };

        let cli_nocolor = Options {
            path: PathBuf::from("."),
            nocolor: true,
            ..Default::default()
        };

        assert!(tree(&dir, &cli_colorful).contains("[1;34m"),);
        assert!(!tree(&dir, &cli_nocolor).contains("[1;34m"),);
    }

    #[test]
    /// Verify that charset set as ascii renders with ascii characters.
    fn test_render_tree_ascii() {
        let dir = create_directory_tree();

        let cli = Options {
            path: PathBuf::from("."),
            charset: SignType::Ascii,
            noreport: true,
            nocolor: true,
            ..Default::default()
        };

        let out = tree(&dir, &cli);
        assert_eq!(
            out,
            format!(
                "{}{}",
                dir.path().to_str().unwrap(),
                expected_output_ascii()
            )
        );
    }

    #[test]
    /// Verify that a full filepath is printed when this option is set.
    fn test_full_path() {
        let dir = create_directory_tree();

        let cli = Options {
            path: PathBuf::from("."),
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
    /// Verify that with the noindent option no tree lines and indentations are drawn.
    fn test_noindent() {
        let cli = Options {
            path: PathBuf::from("."),
            noindent: true,
            noreport: true,
            nocolor: true,
            ..Default::default()
        };
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::create_dir_all(dir.join("foo/bar/baz")).unwrap();

        let tree_out = tree(&dir, &cli);
        println!("{}", tree_out);
        assert!(tree_out.ends_with("foo\nbar\nbaz"));
    }

    #[test]
    /// Verify that inodes are displayed when requested.
    ///
    /// Only display inode and filename without the treelines to make testing easier.
    fn test_inodes_noindent() {
        let cli = Options {
            path: PathBuf::from("."),
            noindent: true,
            noreport: true,
            nocolor: true,
            inode: true,
            ..Default::default()
        };
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        fs::create_dir_all(dir.join("foo/bar/baz")).unwrap();

        let tree_out = tree(&dir, &cli);
        for line in tree_out.lines() {
            // verify that first element of line is u64, so inode
            line.split_whitespace()
                .next()
                .unwrap()
                .parse::<u64>()
                .unwrap();
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
    /// Verify that a missing start directory is handled.
    fn test_missing_start_directory() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path().join("non-existing");

        let cli = Options {
            path: dir.clone(),
            noreport: true,
            nocolor: true,
            ..Default::default()
        };
        let out = tree(&dir, &cli);
        print!("{out}");
        assert!(out.ends_with("/non-existing\n└─ [Cannot access directory: entity not found]"));
    }

    #[test]
    /// Verify correct instantiation of a TreeLevelMeta struct from existing data.
    fn test_tree_level_meta_construct() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let path = tmpdir.path();

        let options = Options {
            ..Default::default()
        };

        let tree_level_meta = TreeLevelMeta::from(&path, &options);
        assert_eq!(None, tree_level_meta.user);
        assert_eq!(None, tree_level_meta.group);

        let tree_level_meta = TreeLevelMeta::from(
            &path,
            &Options {
                user: true,
                ..Default::default()
            },
        );
        assert!(tree_level_meta.user.is_some());
        assert_eq!(None, tree_level_meta.group);

        let tree_level_meta = TreeLevelMeta::from(
            &path,
            &Options {
                group: true,
                ..Default::default()
            },
        );
        assert_eq!(None, tree_level_meta.user);
        assert!(tree_level_meta.group.is_some());

        let tree_level_meta = TreeLevelMeta::from(
            &path,
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
            &path,
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
    /// Verify that dangling symlinks are handled and there metadata is extracted correctly.
    fn test_tree_level_meta_dangling_symlink() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dangling = tmpdir.path().join("dangling");
        std::os::unix::fs::symlink("target", &dangling).unwrap();

        let tree_level_meta = TreeLevelMeta::from(
            &dangling,
            &Options {
                protections: true,
                user: true,
                group: true,
                filetype: true,
                ..Default::default()
            },
        );
        assert!(tree_level_meta.chmods.is_some());
        assert!(tree_level_meta.user.is_some());
        assert!(tree_level_meta.group.is_some());
        assert_eq!(tree_level_meta.filetype.unwrap(), "inode/symlink");
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
                        size: Some(0),
                        ..Default::default()
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
                        size: Some(0),
                        ..Default::default()
                    },
                    levels: vec![],
                    children: TreeChild::Children(vec![TreeEntry {
                        name: PathBuf::from("Last and Best!"),
                        kind: TreeEntryKind::Directory,
                        meta: TreeLevelMeta {
                            chmods: None,
                            group: Some("short".to_string()),
                            user: Some("not the shortest".to_string()),
                            size: Some(0),
                            ..Default::default()
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

    fn render_tree_level_to_string(
        entry: &TreeEntry,
        options: &Options,
        sizes: &TreeEntryLengths,
        lscolors: &LsColors,
    ) -> String {
        let mut out: Vec<u8> = Vec::new();
        render_tree_level(entry, options, sizes, lscolors, &mut out);
        String::from_utf8_lossy(&out).to_string()
    }

    /// Create a directory tree with a directory for which access is restricted.
    fn create_directoy_no_permissions() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        // let dir = std::path::Path::new("/tmp/noperms");
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

    /// Create a directory tree with files that contain various texts.
    ///
    /// Files will be named after their content. Insert normal text in files as well as text that
    /// cannot be rendered or no text at all.
    fn create_directory_tree_texts() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();

        fs::create_dir_all(dir.join("sub/twosub")).unwrap();

        fs::write(dir.join("sub/empty.txt"), "").unwrap();

        fs::write(
            dir.join("sub/normal.txt"),
            "Als Gregor Samsa eines Morgens aus unruhigen Träumen
erwachte, fand er sich in seinem Bett zu einem ungeheueren Ungeziefer verwandelt.
Er lag auf seinem panzerartig harten Rücken und sah, wenn er den Kopf ein wenig
hob, seinen gewölbten, braunen, von bogenförmigen Versteifungen geteilten Bauch,
auf dessen Höhe sich die Bettdecke, zum gänzlichen Niedergleiten bereit, kaum
noch erhalten konnte. Seine vielen, im Vergleich zu seinem sonstigen Umfang
kläglich dünnen Beine flimmerten ihm hilflos vor den Augen.",
        )
        .unwrap();

        fs::write(dir.join("sub/normal_paragraphs.txt"),
"»Was ist mit mir geschehen?«, dachte er. Es war kein Traum. Sein Zimmer, ein richtiges, nur etwas
zu kleines Menschenzimmer, lag ruhig zwischen den vier wohlbekannten Wänden. Über dem Tisch, auf
dem eine auseinandergepackte Musterkollektion von Tuchwaren ausgebreitet war – Samsa war Reisender
– hing das Bild, das er vor kurzem aus einer illustrierten Zeitschrift ausgeschnitten und in einem
hübschen, vergoldeten Rahmen untergebracht hatte. Es stellte eine Dame dar, die mit einem Pelzhut
und einer Pelzboa versehen, aufrecht dasaß und einen schweren Pelzmuff, in dem ihr ganzer Unterarm
verschwunden war, dem Beschauer entgegenhob.

Gregors Blick richtete sich dann zum Fenster, und das trübe Wetter – man hörte Regentropfen auf das
Fensterblech aufschlagen – machte ihn ganz melancholisch. »Wie wäre es, wenn ich noch ein wenig
weiterschliefe und alle Narrheiten vergäße«, dachte er, aber das war gänzlich undurchführbar, denn
er war gewöhnt, auf der rechten Seite zu schlafen, konnte sich aber in seinem gegenwärtigen Zustand
nicht in diese Lage bringen. Mit welcher Kraft er sich auch auf die rechte Seite warf, immer wieder
schaukelte er in die Rückenlage zurück. Er versuchte es wohl hundertmal, schloß die Augen, um die
zappelnden Beine nicht sehen zu müssen, und ließ erst ab, als er in der Seite einen noch nie
gefühlten, leichten, dumpfen Schmerz zu fühlen begann.

»Ach Gott«, dachte er, »was für einen anstrengenden Beruf habe ich gewählt! Tag aus, Tag ein auf
der Reise. Die geschäftlichen Aufregungen sind viel größer, als im eigentlichen Geschäft zu Hause,
und außerdem ist mir noch diese Plage des Reisens auferlegt, die Sorgen um die Zuganschlüsse, das
unregelmäßige, schlechte Essen, ein immer wechselnder, nie andauernder, nie herzlich werdender
menschlicher Verkehr. Der Teufel soll das alles holen!« Er fühlte ein leichtes Jucken oben auf dem
Bauch; schob sich auf dem Rücken langsam näher zum Bettpfosten, um den Kopf besser heben zu können;
fand die juckende Stelle, die mit lauter kleinen weißen Pünktchen besetzt war, die er nicht zu
beurteilen verstand; und wollte mit einem Bein die Stelle betasten, zog es aber gleich zurück, denn
bei der Berührung umwehten ihn Kälteschauer.").unwrap();

        fs::write(
            dir.join("sub/twosub/nonprintable_mixed_in.txt"),
            "Er glitt wieder in seine frühere Lage zurück.

»Dies frühzeitige Aufstehen«, dachte er, »macht einen ganz blödsinnig.

Der Mensch muß seinen Schlaf haben.
Andere Reisende leben wie Haremsfrauen.

Wenn ich zum Beispiel im Laufe des Vormittags ins Gasthaus zurückgehe, um die erlangten
Aufträge zu überschreiben, sitzen diese Herren erst beim Frühstück.",
        )
        .unwrap();

        fs::write(
            dir.join("sub/twosub/nonprintable_complete.txt"),
            "
",
        )
        .unwrap();

        fs::write(dir.join("sub/twosub/printable_emojies.txt"), "😳😄").unwrap();

        fs::write(
            dir.join("x_final.txt"),
"Das sollte ich bei meinem Chef versuchen; ich würde auf der Stelle hinausfliegen. Wer weiß
übrigens, ob das nicht sehr gut für mich wäre. Wenn ich mich nicht wegen meiner Eltern
zurückhielte, ich hätte längst gekündigt, ich wäre vor den Chef hin getreten und hätte ihm meine
Meinung von Grund des Herzens aus gesagt. Vom Pult hätte er fallen müssen!").unwrap();

        //fs::write(dir.join("missing_perms.txt"), "stay away?").unwrap();
        //fs::set_permissions(dir.join("missing_perms.txt"), fs::Permissions::from_mode(0o000)).unwrap();

        tmpdir
    }

    /// Output that should be generated when using the directory tree with texts.
    fn expected_output_directory_tree_text() -> String {
        let output: String = "
├─sub
│   ├─empty.txt
│   ├─normal.txt
│   │   Als Gregor Samsa eines Morgens aus unruhigen Träumen
│   │   erwachte, fand er sich in seinem Bett zu einem ungeheueren Ungeziefer verwandelt.
│   │   Er lag auf seinem panzerartig harten Rücken und sah, wenn er den Kopf ein wenig
│   │   hob, seinen gewölbten, braunen, von bogenförmigen Versteifungen geteilten Bauch,
│   │   auf dessen Höhe sich die Bettdecke, zum gänzlichen Niedergleiten bereit, kaum
│   │   noch erhalten konnte. Seine vielen, im Vergleich zu seinem sonstigen Umfang
│   │   kläglich dünnen Beine flimmerten ihm hilflos vor den Augen.
│   ├─normal_paragraphs.txt
│   │   »Was ist mit mir geschehen?«, dachte er. Es war kein Traum. Sein Zimmer, ein richtiges, nur etwas
│   │   zu kleines Menschenzimmer, lag ruhig zwischen den vier wohlbekannten Wänden. Über dem Tisch, auf
│   │   dem eine auseinandergepackte Musterkollektion von Tuchwaren ausgebreitet war – Samsa war Reisender
│   │   – hing das Bild, das er vor kurzem aus einer illustrierten Zeitschrift ausgeschnitten und in einem
│   │   hübschen, vergoldeten Rahmen untergebracht hatte. Es stellte eine Dame dar, die mit einem Pelzhut
│   │   und einer Pelzboa versehen, aufrecht dasaß und einen schweren Pelzmuff, in dem ihr ganzer Unterarm
│   │   verschwunden war, dem Beschauer entgegenhob.
│   │   
│   │   Gregors Blick richtete sich dann zum Fenster, und das trübe Wetter – man hörte Regentropfen auf das
│   │   Fensterblech aufschlagen – machte ihn ganz melancholisch. »Wie wäre es, wenn ich noch ein wenig
│   │   weiterschliefe und alle Narrheiten vergäße«, dachte er, aber das war gänzlich undurchführbar, denn
│   │   er war gewöhnt, auf der rechten Seite zu schlafen, konnte sich aber in seinem gegenwärtigen Zustand
│   │   nicht in diese Lage bringen. Mit welcher Kraft er sich auch auf die rechte Seite warf, immer wieder
│   │   schaukelte er in die Rückenlage zurück. Er versuchte es wohl hundertmal, schloß die Augen, um die
│   │   zappelnden Beine nicht sehen zu müssen, und ließ erst ab, als er in der Seite einen noch nie
│   │   gefühlten, leichten, dumpfen Schmerz zu fühlen begann.
│   │   
│   │   »Ach Gott«, dachte er, »was für einen anstrengenden Beruf habe ich gewählt! Tag aus, Tag ein auf
│   │   der Reise. Die geschäftlichen Aufregungen sind viel größer, als im eigentlichen Geschäft zu Hause,
│   │   und außerdem ist mir noch diese Plage des Reisens auferlegt, die Sorgen um die Zuganschlüsse, das
│   │   unregelmäßige, schlechte Essen, ein immer wechselnder, nie andauernder, nie herzlich werdender
│   │   menschlicher Verkehr. Der Teufel soll das alles holen!« Er fühlte ein leichtes Jucken oben auf dem
│   │   Bauch; schob sich auf dem Rücken langsam näher zum Bettpfosten, um den Kopf besser heben zu können;
│   │   fand die juckende Stelle, die mit lauter kleinen weißen Pünktchen besetzt war, die er nicht zu
│   │   beurteilen verstand; und wollte mit einem Bein die Stelle betasten, zog es aber gleich zurück, denn
│   │   bei der Berührung umwehten ihn Kälteschauer.
│   └─twosub
│       ├─nonprintable_complete.txt
│       ├─nonprintable_mixed_in.txt
│       │   Er glitt wieder in seine frühere Lage zurück.
│       │   »Dies frühzeitige Aufstehen«, dachte er, »macht einen ganz blödsinnig.
│       │   Der Mensch muß seinen Schlaf haben.
│       │   Andere Reisende leben wie Haremsfrauen.
│       │   Wenn ich zum Beispiel im Laufe des Vormittags ins Gasthaus zurückgehe, um die erlangten
│       │   Aufträge zu überschreiben, sitzen diese Herren erst beim Frühstück.
│       └─printable_emojies.txt
│           😳😄
└─x_final.txt
    Das sollte ich bei meinem Chef versuchen; ich würde auf der Stelle hinausfliegen. Wer weiß
    übrigens, ob das nicht sehr gut für mich wäre. Wenn ich mich nicht wegen meiner Eltern
    zurückhielte, ich hätte längst gekündigt, ich wäre vor den Chef hin getreten und hätte ihm meine
    Meinung von Grund des Herzens aus gesagt. Vom Pult hätte er fallen müssen!".to_string();

        output
    }

    /// Output that should be generated when using the directory tree with tail -1.
    fn expected_output_directory_tree_tail_three() -> String {
        let output: String = "
├─sub
│   ├─empty.txt
│   ├─normal.txt
│   │   auf dessen Höhe sich die Bettdecke, zum gänzlichen Niedergleiten bereit, kaum
│   │   noch erhalten konnte. Seine vielen, im Vergleich zu seinem sonstigen Umfang
│   │   kläglich dünnen Beine flimmerten ihm hilflos vor den Augen.
│   ├─normal_paragraphs.txt
│   │   fand die juckende Stelle, die mit lauter kleinen weißen Pünktchen besetzt war, die er nicht zu
│   │   beurteilen verstand; und wollte mit einem Bein die Stelle betasten, zog es aber gleich zurück, denn
│   │   bei der Berührung umwehten ihn Kälteschauer.
│   └─twosub
│       ├─nonprintable_complete.txt
│       ├─nonprintable_mixed_in.txt
│       │   Wenn ich zum Beispiel im Laufe des Vormittags ins Gasthaus zurückgehe, um die erlangten
│       │   Aufträge zu überschreiben, sitzen diese Herren erst beim Frühstück.
│       └─printable_emojies.txt
│           😳😄
└─x_final.txt
    übrigens, ob das nicht sehr gut für mich wäre. Wenn ich mich nicht wegen meiner Eltern
    zurückhielte, ich hätte längst gekündigt, ich wäre vor den Chef hin getreten und hätte ihm meine
    Meinung von Grund des Herzens aus gesagt. Vom Pult hätte er fallen müssen!".to_string();

        output
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
            fs::write(dir.join(file), file).unwrap(); // write file path to file
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
│       ├─autumn.jpg
│       ├─spring.gif
│       ├─summer.png
│       └─winter.png
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
│       ├─autumn.jpg
│       ├─spring.gif
│       ├─summer.png
│       └─winter.png
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

    /// The expected output for the directory tree tests with file sizes.
    fn expected_output_standard_sizes() -> String {
        let output: String = "
7    ├─bar.txt
4096 ├─Desktop
4096 ├─Downloads
37   │   ├─cargo_0.57.0-7+b1_amd64.deb
20   │   ├─cygwin.exe
40   │   └─rustc_1.60.0+dfsg1-1_amd64.deb
7    ├─foo.txt
4096 ├─Music
13   │   ├─one.mp3
15   │   ├─three.mp3
13   │   └─two.mp3
4096 ├─My Projects
4096 ├─Pictures
4096 │   ├─days
25   │   │   ├─evening.bmp
26   │   │   ├─morning.tiff
22   │   │   └─noon.svg
18   │   ├─hello.png
4096 │   └─seasons
27   │       ├─autumn.jpg
27   │       ├─spring.gif
27   │       ├─summer.png
27   │       └─winter.png
4096 └─Trash
12       ├─bar.md
13       ├─foo.txt
4096     └─old
17           ├─bar.txt
17           ├─baz.txt
16           ├─foo.md
4096         └─obsolete
26               ├─does.md
27               ├─tres.txt
25               └─uno.md"
            .to_string();
        output
    }

    /// The expected output for the directory tree tests with file sizes.
    fn expected_output_standard_sizes_human() -> String {
        let output: String = "
7 B   ├─bar.txt
4 KiB ├─Desktop
4 KiB ├─Downloads
37 B  │   ├─cargo_0.57.0-7+b1_amd64.deb
20 B  │   ├─cygwin.exe
40 B  │   └─rustc_1.60.0+dfsg1-1_amd64.deb
7 B   ├─foo.txt
4 KiB ├─Music
13 B  │   ├─one.mp3
15 B  │   ├─three.mp3
13 B  │   └─two.mp3
4 KiB ├─My Projects
4 KiB ├─Pictures
4 KiB │   ├─days
25 B  │   │   ├─evening.bmp
26 B  │   │   ├─morning.tiff
22 B  │   │   └─noon.svg
18 B  │   ├─hello.png
4 KiB │   └─seasons
27 B  │       ├─autumn.jpg
27 B  │       ├─spring.gif
27 B  │       ├─summer.png
27 B  │       └─winter.png
4 KiB └─Trash
12 B      ├─bar.md
13 B      ├─foo.txt
4 KiB     └─old
17 B          ├─bar.txt
17 B          ├─baz.txt
16 B          ├─foo.md
4 KiB         └─obsolete
26 B              ├─does.md
27 B              ├─tres.txt
25 B              └─uno.md"
            .to_string();
        output
    }

    /// The expected output for the directory tree tests with filetype identification
    fn expected_output_filetypes() -> String {
        let output: String = "
text/plain      ├─bar.txt
inode/directory ├─Desktop
inode/directory ├─Downloads
text/plain      │   ├─cargo_0.57.0-7+b1_amd64.deb
text/plain      │   ├─cygwin.exe
text/plain      │   └─rustc_1.60.0+dfsg1-1_amd64.deb
text/plain      ├─foo.txt
inode/directory ├─Music
text/plain      │   ├─one.mp3
text/plain      │   ├─three.mp3
text/plain      │   └─two.mp3
inode/directory ├─My Projects
inode/directory ├─Pictures
inode/directory │   ├─days
text/plain      │   │   ├─evening.bmp
text/plain      │   │   ├─morning.tiff
text/plain      │   │   └─noon.svg
text/plain      │   ├─hello.png
inode/directory │   └─seasons
text/plain      │       ├─autumn.jpg
text/plain      │       ├─spring.gif
text/plain      │       ├─summer.png
text/plain      │       └─winter.png
inode/directory └─Trash
text/plain          ├─bar.md
text/plain          ├─foo.txt
inode/directory     └─old
text/plain              ├─bar.txt
text/plain              ├─baz.txt
text/plain              ├─foo.md
inode/directory         └─obsolete
text/plain                  ├─does.md
text/plain                  ├─tres.txt
text/plain                  └─uno.md"
            .to_string();
        output
    }

    /// The expected output if ascii charset is chosen.
    fn expected_output_ascii() -> String {
        let output: String = "
|-bar.txt
|-Desktop
|-Downloads
|   |-cargo_0.57.0-7+b1_amd64.deb
|   |-cygwin.exe
|   `-rustc_1.60.0+dfsg1-1_amd64.deb
|-foo.txt
|-Music
|   |-one.mp3
|   |-three.mp3
|   `-two.mp3
|-My Projects
|-Pictures
|   |-days
|   |   |-evening.bmp
|   |   |-morning.tiff
|   |   `-noon.svg
|   |-hello.png
|   `-seasons
|       |-autumn.jpg
|       |-spring.gif
|       |-summer.png
|       `-winter.png
`-Trash
    |-bar.md
    |-foo.txt
    `-old
        |-bar.txt
        |-baz.txt
        |-foo.md
        `-obsolete
            |-does.md
            |-tres.txt
            `-uno.md"
            .to_string();
        output
    }
}
