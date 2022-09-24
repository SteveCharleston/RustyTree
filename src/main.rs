use clap::Parser;
use rustytree::{print_paths, TreeLevel};
use std::io;

#[derive(Debug, Parser)]
#[clap(
    about = "List contents of directories in a tree-like format",
    author = "Steven Schalhorn <steven@schalhorn.org>"
)]
struct Cli {
    #[clap(default_value_t = String::from("."), value_parser)]
    path: String,
}

fn main() {
    let cli = Cli::parse();
    // println!("{:?}", cli);

    let mut indent: Vec<TreeLevel> = Vec::new();
    println!("{}", cli.path);
    print_paths(cli.path.as_str(), &mut indent, &mut io::stdout());

    // let mut output: Vec<u8> = Vec::new();
    // print_paths(cli.path.as_str(), &mut indent, &mut output);
    // let output_string = String::from_utf8_lossy(&output);
    // println!("{}", output_string);
}
