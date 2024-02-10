//! Collect all structs that represent parts of the file tree.

use super::determine_filetype;
use super::errors;
use super::Options;
use rayon::prelude::*;
use std::fs;
use std::os::unix::prelude::MetadataExt;
use std::path::{Path, PathBuf};

/// Represent the different possible indentation components of a file.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TreeLevel {
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
pub struct TreeEntry {
    /// Name of the current file or directory
    pub name: PathBuf,

    /// Save kind of entry to display it differently
    pub kind: TreeEntryKind,

    /// Metadata about the file or directory
    pub meta: TreeLevelMeta,

    /// List of different levels of parent directories up to the root
    pub levels: Vec<TreeLevel>,

    /// List of child entries to enable a recursive data structure
    pub children: TreeChild,
}

impl TreeEntry {
    /// Calculate the length of the longest field.
    ///
    /// Goes through the whole tree and subtrees and looks at the given field for every node to
    /// determine the length of the longest entry. Return those length to enable better formatting
    /// with this information.
    pub fn longest_fieldentry(
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
pub struct TreeLevelMeta {
    /// Chmods of file or directory
    pub chmods: Option<u32>,

    /// Username of file or directory
    pub user: Option<String>,

    /// Group name of file or directory
    pub group: Option<String>,

    /// Size of file or collectively of all subfiles of a directory
    pub size: Option<u64>,

    /// Unique Identifier of the file or directory in form of InodeData
    pub inode: Option<InodeData>,

    /// Type of the content inside the file
    pub filetype: Option<String>,

    /// External program output used on the file
    pub exec: Option<Result<String, std::io::ErrorKind>>,
}

impl TreeLevelMeta {
    /// Generate a new TreeLevelMeta from the given data metadata
    pub fn from(path: &impl AsRef<Path>, options: &Options) -> TreeLevelMeta {
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
pub struct InodeData {
    /// Inode number on the filesystem
    pub inode: u64,
    /// ID of the device that contains the file
    pub dev: u64,
}

/// Cache the lengths of some TreeEntry fields to avoid recalculating them during drawing.
#[derive(Default)]
pub struct TreeEntryLengths {
    /// Length of the longest user field
    pub user: usize,

    /// Length of the longest group field
    pub group: usize,

    /// Length of the longest size field
    pub size: usize,

    /// Lenght of the longest filetype field
    pub filetype: usize,
}

/// Hold the rendered tree as well as number of directories and files to generate the final status
/// line.
#[derive(Clone, Debug)]
pub struct TreeRepresentation {
    /// Final rendered string representation of the tree
    pub rendered: String,
    /// Number of directories in the rendered tree
    pub directories: u32,
    /// Number of files in the rendered tree
    pub files: u32,
    /// Size of all files combined, if requested
    pub size: Option<String>,
}

impl TreeRepresentation {
    /// Format information about number of directories, files and possible size as report string.
    pub fn statistics(&self) -> String {
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
pub enum TreeChild {
    /// No children exist or have been read yet
    None,
    /// Directory couldn't be accessed for some reason
    Error(errors::TreeError),
    /// The expected child entries
    Children(Vec<TreeEntry>),
}

/// Represent which kind of file a TreeEntry is.
#[derive(Clone, Debug)]
pub enum TreeEntryKind {
    /// TreeEntry is a regular file
    File,
    /// TreeEntry is a Directory
    Directory,
    /// TreeEntry is a Symlink
    Symlink(Box<TreeEntryKind>),
}

#[cfg(test)]
mod tests {
    use super::*;

}
