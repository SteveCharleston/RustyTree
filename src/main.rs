use clap::Parser;
use rustytree::tree_writer;
use rustytree::Options;
use std::io;

fn main() {
    let cli = Options::parse();
    let mut output = io::stdout();

    tree_writer(&cli.path, &cli, &mut output);
}
