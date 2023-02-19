//! Helper functionality to ease the handling and displaying of errors.
//!
//! Mostly wraps the existing std::io:ErrorKind errors, which fit in nearly all situations.
//! Also provide some more error types for specific errors that might arise in special
//! situations that are not covered by the generic crate. use std::io;

use std::{fmt, io};

/// Provide an enum for all possible errors that might arise in this program.
#[derive(Clone, Debug)]
pub enum TreeError {
    /// Loop in the filesystem constructed with symbolic links.
    SymlinkLoop,
    /// Directory lies on a different filesystem than the one the program was started on.
    FilesystemBoundary,
    /// General IO errors that are wrapped in this enum.
    IoError(io::ErrorKind),
}

impl fmt::Display for TreeError {
    /// Shows a human-readable description of the `TreeError`.
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            TreeError::SymlinkLoop => fmt.write_str("Symlink loop detected"),
            TreeError::FilesystemBoundary => fmt.write_str("Not crossing filesystem boundary"),
            TreeError::IoError(kind) => fmt.write_str(kind.to_string().as_str()),
        }
    }
}
