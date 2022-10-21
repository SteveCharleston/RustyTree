use rustytree::tree;
use rustytree::Options;
use clap::Parser;

fn main() {
    let cli = Options::parse();

    println!("{}", tree(&cli.path, &cli));
}
