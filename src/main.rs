use clap::Parser;
use rustytree::tree_writer;
use rustytree::options::Options;
use std::io;

#[cfg(not(tarpaulin_include))]
fn main() {
    let cli = Options::parse();
    let mut output = io::stdout();

    tree_writer(&cli.path, &cli, &mut output);
}
