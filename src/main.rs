use clap::Parser;
use rustytree::tree;

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

    println!("{}", cli.path);
    println!("{}", tree(&cli.path));
}
