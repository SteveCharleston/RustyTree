//! Collect all functions and structs that are used to parse the command line arguments.
use clap::Parser;
use globset::{Glob, GlobSet, GlobSetBuilder};
use std::path::PathBuf;

#[derive(Debug, Default, Parser)]
#[clap(
    about = "List contents of directories in a tree-like format",
    author = "Steven Schalhorn <steven@schalhorn.org>"
)]
/// Arguments to the application.
pub struct Options {
    #[clap(default_value = ".", value_parser)]
    /// Path to the directory to traverse into
    pub path: PathBuf,

    #[clap(short = 'F', long)]
    /// Append a `/' for directories
    pub classify: bool,

    #[clap(short = 'n', long)]
    /// Turn colorization off
    pub nocolor: bool,

    #[clap(short = 'f', long)]
    /// Print the full path prefix for each file
    pub fullpath: bool,

    #[clap(short = 'i', long)]
    /// Do not print indentation and tree lines
    pub noindent: bool,

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

    #[clap(short = 's', long)]
    /// Show the size of a file or colelctively of all subfiles of a directory
    pub sizes: bool,

    #[clap(long)]
    /// Print the sizes in a human readable way
    pub humansize: bool,

    #[clap(long)]
    /// Print the sizes in a human readable way with SI sizes
    pub si: bool,

    #[clap(long)]
    /// Recursively sum the sizes of files and subdirectories and display that on a folder
    pub du: bool,

    #[clap(short = 't', long)]
    /// Determine the type of every file
    pub filetype: bool,

    #[clap(long, value_parser = parse_pattern)]
    /// List only files that match the given filetype pattern, separate multiple patterns by pipe |
    pub filtertype: Option<GlobSet>,

    #[clap(short = 'P', long, value_parser = parse_pattern)]
    /// List only files that match the given pattern, separate multiple patterns by pipe |
    pub pattern: Option<GlobSet>,

    #[clap(short = 'I', long, value_parser = parse_pattern)]
    /// Do not files that match the given pattern, separate multiple patterns by pipe |
    pub inversepattern: Option<GlobSet>,

    #[clap(long)]
    /// Prune empty directories from the output
    pub prune: bool,

    #[clap(short = 'x', long)]
    /// Stay on the filesystem of the given path
    pub xdev: bool,

    #[clap(long)]
    /// Print the inode number of the file or directory
    pub inode: bool,

    #[clap(short = 'l', long)]
    /// Follow symbolic links into directories
    pub followlinks: bool,

    #[clap(value_enum, short = 'S', long, default_value_t=SignType::Ucs)]
    /// Follow symbolic links into directories
    pub charset: SignType,

    #[clap(long)]
    /// Output the file contents
    pub cat: bool,

    #[clap(long, value_parser = ExecCommand::from)]
    /// Execute the given command with {} replaced as filename
    pub exec: Option<ExecCommand>,
}

///
/// Take a list of Glob pattern and create a GlobSet out of them.
///
/// The list of pattern is separated by a pipe symbol '|' and all supplied patterns are stored in
/// the GlobSet. In case of a wrong pattern we will get an error that describes the problem.
pub fn parse_pattern(arg: &str) -> Result<GlobSet, globset::Error> {
    let mut globset = GlobSetBuilder::new();
    for sub_glob in arg.split('|') {
        let glob = Glob::new(sub_glob)?;
        globset.add(glob);
    }
    globset.build()
}

/// Used to encode how the tree lines should be drawn
#[derive(clap::ValueEnum, Clone, Debug, Default)]
pub enum SignType {
    /// Unicode Characters
    #[default]
    Ucs,
    /// ASCII Characters
    Ascii,
}

#[derive(Clone, Debug)]
/// Represent a system command that will be executed.
pub struct ExecCommand {
    /// The command to call with the arguments
    pub command: String,
    /// The arguments that are supplied to the given command
    pub arguments: Vec<String>,
}

impl ExecCommand {
    /// Take a command string and create a ExecCommand to be used for command execution.
    pub fn from(arg: &str) -> Result<ExecCommand, shell_words::ParseError> {
        let cmd_shell_split = shell_words::split(arg)?;
        let cmd = cmd_shell_split.first();
        let args = cmd_shell_split.get(1..);
        Ok(ExecCommand {
            command: cmd.unwrap_or(&String::from("")).to_string(),
            arguments: args.unwrap_or(&[]).to_vec(),
        })
    }
}
