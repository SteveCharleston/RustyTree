use clap::Parser;
use rustytree::tree;
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

    println!("{}", cli.path);

    let output = io::stdout();
    tree(&cli.path, output);

    // let mut output: Vec<u8> = Vec::new();
    // tree(cli.path.as_str(), &mut indent, &mut output);
    // let output_string = String::from_utf8_lossy(&output);
    // println!("{}", output_string);
}
