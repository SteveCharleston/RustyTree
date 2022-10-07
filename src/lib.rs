//! This crate provides a library to render a graphical representation of a filesystem in a tree
//! like fashion.
//!
//! Each directory is entered in an own thread to increase speed when concurrently traversing the
//! directory tree. Additionally expensive computations can be done like the hash sum generation of
//! files, which will also be done in a separate thread per file.

#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

use rayon::prelude::*;
use std::fs::DirEntry;
use std::path::Path;
use std::{fs, io};

/// Indentation if no parent exists
const INDENT_SIGN: &str = "  ";

/// Bar if a parent exists
const TREE_SIGN: &str = "│ ";

/// In front of a file or dir if it is not the last
const INNER_BRANCH: &str = "├─";

/// In front of a file or dir if it is the last
const FINAL_BRANCH: &str = "└─";

/// Represent the different possible indentation components of a file.
#[derive(Clone)]
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
#[derive(Clone)]
struct TreeEntry {
    /// Name of the current file or directory
    name: String,

    /// List of different levels of parent directories up to the root
    levels: Vec<TreeLevel>,
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
fn read_dir_sorted(path: &impl AsRef<Path>) -> Result<Vec<DirEntry>, io::Error> {
    // dbg!(PathBuf::from(path.as_ref()));
    let mut paths: Vec<_> = fs::read_dir(path)?
        .map(|r| r.expect("Reading file inside a directory")) // not expected to normally fail
        .collect();

    paths.sort_by_key(|entry| entry.path().to_string_lossy().to_lowercase());
    Ok(paths)
}

/// Render the given TreeEntry into a string representation.
fn render_tree_level(entry: &TreeEntry) -> String {
    let mut rendered_entry = String::new();

    for level in &entry.levels {
        let current_level = match level {
            TreeLevel::Indent => INDENT_SIGN.to_string(),
            TreeLevel::TreeBar => TREE_SIGN.to_string() + INDENT_SIGN,
            TreeLevel::TreeBranch => INNER_BRANCH.to_string(),
            TreeLevel::TreeFinalBranch => FINAL_BRANCH.to_string(),
        };
        rendered_entry += current_level.as_str();
    }

    rendered_entry += entry.name.as_str();
    rendered_entry += "\n";
    rendered_entry
}

/// Generate a tree representation of the filesystem.
///
/// Walk the filesystem starting from the given directory and visit all child directories and files
/// recursively. Render all the files into a tree like string representation. Each directory visit
/// is done in a thread and also some expensive computations might be executed which will also be
/// threaded to distribute the load amongst the available cores.
pub fn tree(path: &impl AsRef<Path>) -> String {
    let indent_level: Vec<TreeLevel> = Vec::new();

    print_paths(path, &indent_level)
}

/// Actually do the work of computing the tree.
fn print_paths(
    path: &impl AsRef<Path>,
    indent_level: &[TreeLevel],
) -> String {
    let entries = match read_dir_sorted(path) {
        Ok(entries) => entries,
        // return nothing since we don't have anything to display. Could be used to display such
        // information in future.
        Err(_) => return "".to_string(),
    };
    let entries_len = entries.len();
    entries
        .into_par_iter()
        .enumerate()
        .map(|(i, entry)| {
            let mut out = String::new();
            let mut current_indent: Vec<TreeLevel> = indent_level.to_vec();
            let mut recurisve_indent: Vec<TreeLevel> = indent_level.to_vec();

            if i == entries_len - 1 {
                current_indent.push(TreeLevel::TreeFinalBranch);
                recurisve_indent.push(TreeLevel::Indent);
            } else {
                current_indent.push(TreeLevel::TreeBranch);
                recurisve_indent.push(TreeLevel::TreeBar);
            };

            let tree_entry = TreeEntry {
                name: entry.file_name().to_string_lossy().to_string(),
                levels: current_indent.to_vec(),
            };
            let tree_level = render_tree_level(&tree_entry);

            out += tree_level.as_str();


            if entry.path().is_dir() {
                out += print_paths(&entry.path(), &recurisve_indent).as_str()
            }

            out
        })
        .collect::<Vec<String>>()
        .join("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::fs::File;
    use std::os::unix::prelude::PermissionsExt;
    use tempfile;

    #[test]
    /// Verify that a generated filesystem tree is as expected.
    fn test_print_paths() {
        let dir = create_directory_tree();
        let mut indent: Vec<TreeLevel> = Vec::new();

        println!("tmpdir: {:?}", dir);
        let out = print_paths(&dir.path(), &mut indent);

        println!("{}", out);
        assert_eq!(out, expected_output_standard());
    }

    #[test]
    /// Verfiy that missing read permissions are handled gracefully.
    fn test_no_read_permissions() {
        let mut indent: Vec<TreeLevel> = Vec::new();

        let dir = create_directoy_no_permissions();
        let out = print_paths(&dir.path(), &mut indent);

        println!("{}", out);
        assert_eq!(out, "└─root\n");
    }

    #[test]
    /// Verify that a directory is sorted correctly.
    fn test_read_dir_sorted() {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();

        let sorted_dirs = ["Bardir", "does", "Foodir", "tres", "unos", "Xanadu"];

        fs::create_dir_all(dir).unwrap();

        fs::create_dir(dir.join("Foodir")).unwrap();
        fs::create_dir(dir.join("Bardir")).unwrap();
        fs::create_dir(dir.join("Xanadu")).unwrap();
        fs::create_dir(dir.join("unos")).unwrap();
        fs::create_dir(dir.join("does")).unwrap();
        fs::create_dir(dir.join("tres")).unwrap();

        let entries = read_dir_sorted(&dir).unwrap();

        for (i, entry) in entries.iter().enumerate() {
            println!("{:?} -- {}", entry.file_name(), sorted_dirs[i]);
            assert_eq!(entry.file_name().into_string().unwrap(), sorted_dirs[i]);
        }
    }

    #[test]
    /// Verify that tree entries from a list of `TreeLevel` enums are rendered correct.
    fn test_render_tree_entry() {
        let test_entries = vec![
            (vec![TreeLevel::TreeBranch], "├─filename\n"),
            (
                vec![TreeLevel::TreeBar, TreeLevel::TreeBranch],
                "│   ├─filename\n",
            ),
            (
                vec![TreeLevel::TreeBar, TreeLevel::TreeFinalBranch],
                "│   └─filename\n",
            ),
            (
                vec![TreeLevel::Indent, TreeLevel::TreeFinalBranch],
                "  └─filename\n",
            ),
            (
                vec![
                    TreeLevel::TreeBar,
                    TreeLevel::TreeBar,
                    TreeLevel::TreeBranch,
                ],
                "│   │   ├─filename\n",
            ),
            (
                vec![
                    TreeLevel::TreeBar,
                    TreeLevel::TreeBar,
                    TreeLevel::TreeFinalBranch,
                ],
                "│   │   └─filename\n",
            ),
        ];

        for (level_data, entry_presentation) in test_entries {
            let entry = TreeEntry {
                name: String::from("filename"),
                levels: level_data,
            };
            assert_eq!(render_tree_level(&entry), entry_presentation);
            print!("{}", render_tree_level(&entry));
        }
    }

    /// Create a directory tree with a directory for which access is restricted.
    fn create_directoy_no_permissions() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        let target_dir = dir.join("root");
        fs::create_dir_all(dir).unwrap();
        fs::create_dir_all(&target_dir).unwrap();

        fs::create_dir_all(target_dir.join("uno")).unwrap();
        File::create(target_dir.join("uno/foo.txt")).unwrap();
        File::create(target_dir.join("uno/bar.txt")).unwrap();

        fs::create_dir_all(target_dir.join("does")).unwrap();
        File::create(target_dir.join("does/baz.md")).unwrap();

        fs::set_permissions(target_dir, fs::Permissions::from_mode(0o000)).unwrap();

        tmpdir
    }

    /// Create a common possible directory tree.
    fn create_directory_tree() -> tempfile::TempDir {
        let tmpdir = tempfile::tempdir().expect("Trying to create a temporary directoy.");
        let dir = tmpdir.path();
        // let dir = std::path::Path::new("/tmp/rustytree");
        fs::create_dir_all(dir).unwrap();

        File::create(dir.join("foo.txt")).unwrap();
        File::create(dir.join("bar.txt")).unwrap();

        fs::create_dir_all(dir.join(".vim/session")).unwrap();
        File::create(dir.join(".vim/vimrc")).unwrap();
        File::create(dir.join(".vim/.netrwhist")).unwrap();
        File::create(dir.join(".vim/session/default.vim")).unwrap();
        File::create(dir.join(".vim/session/testsess.vim")).unwrap();
        File::create(dir.join(".vim/session/worksess.vim")).unwrap();

        fs::create_dir_all(dir.join("Desktop")).unwrap();
        fs::create_dir_all(dir.join("Downloads")).unwrap();
        File::create(dir.join("Downloads/cygwin.exe")).unwrap();
        File::create(dir.join("Downloads/rustc_1.60.0+dfsg1-1_amd64.deb")).unwrap();
        File::create(dir.join("Downloads/cargo_0.57.0-7+b1_amd64.deb")).unwrap();

        fs::create_dir_all(dir.join("Music")).unwrap();
        File::create(dir.join("Music/one.mp3")).unwrap();
        File::create(dir.join("Music/two.mp3")).unwrap();
        File::create(dir.join("Music/three.mp3")).unwrap();

        fs::create_dir_all(dir.join("My Projects")).unwrap();

        fs::create_dir_all(dir.join("Pictures/seasons")).unwrap();
        fs::create_dir_all(dir.join("Pictures/days")).unwrap();
        File::create(dir.join("Pictures/hello.png")).unwrap();
        File::create(dir.join("Pictures/days/evening.bmp")).unwrap();
        File::create(dir.join("Pictures/days/noon.svg")).unwrap();
        File::create(dir.join("Pictures/days/morning.tiff")).unwrap();
        File::create(dir.join("Pictures/seasons/autumn.jpg")).unwrap();
        File::create(dir.join("Pictures/seasons/spring.gif")).unwrap();
        File::create(dir.join("Pictures/seasons/summer.png")).unwrap();
        File::create(dir.join("Pictures/seasons/winter.png")).unwrap();

        fs::create_dir_all(dir.join("Trash/old/obsolete")).unwrap();
        File::create(dir.join("Trash/foo.txt")).unwrap();
        File::create(dir.join("Trash/bar.md")).unwrap();
        File::create(dir.join("Trash/old/bar.txt")).unwrap();
        File::create(dir.join("Trash/old/baz.txt")).unwrap();
        File::create(dir.join("Trash/old/foo.md")).unwrap();
        File::create(dir.join("Trash/old/obsolete/uno.md")).unwrap();
        File::create(dir.join("Trash/old/obsolete/does.md")).unwrap();
        File::create(dir.join("Trash/old/obsolete/tres.txt")).unwrap();

        tmpdir
    }

    /// The expected output for the directory tree tests run agains.
    fn expected_output_standard() -> String {
        let output: String = "\
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
│     ├─autumn.jpg
│     ├─spring.gif
│     ├─summer.png
│     └─winter.png
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
      └─uno.md
"
        .to_string();
        output
    }
}
