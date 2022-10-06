use rayon::prelude::*;
use std::fs;
use std::fs::DirEntry;
use std::io::Write;
use std::path::Path;
use std::sync::{Arc, Mutex};
const INDENT_SIGN: &str = "  ";
const TREE_SIGN: &str = "│ ";
const INNER_BRANCH: &str = "├─";
const FINAL_BRANCH: &str = "└─";

#[derive(Clone)]
pub enum TreeLevel {
    Indent,
    TreeBar,
    TreeBranch,
    TreeFinalBranch,
}

#[derive(Clone)]
pub struct TreeEntry {
    name: String,
    levels: Vec<TreeLevel>,
}

fn read_dir_sorted(path: &impl AsRef<Path>) -> Vec<DirEntry> {
    let mut paths: Vec<_> = fs::read_dir(path).unwrap().map(|r| r.unwrap()).collect();

    paths.sort_by_key(|entry| entry.path().to_str().unwrap().to_lowercase());
    paths
}

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

pub fn tree<W: Write + Send + 'static + Sync>(
    path: &impl AsRef<Path>,
    indent_level: &[TreeLevel],
    output: W,
) {
    let mut threadsafe_output = Arc::new(Mutex::new(output));
    let new_output = Arc::clone(&threadsafe_output);

    let out = print_paths(path, indent_level, &mut threadsafe_output);
    new_output
        .lock()
        .unwrap()
        .write_all(out.as_bytes())
        .unwrap();
}

fn print_paths<W: Write + Send + 'static + Sync>(
    path: &impl AsRef<Path>,
    indent_level: &[TreeLevel],
    writer: &mut Arc<Mutex<W>>,
) -> String {
    let entries = read_dir_sorted(path);
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
                name: entry.file_name().into_string().unwrap(),
                levels: current_indent.to_vec(),
            };
            let tree_level = render_tree_level(&tree_entry);

            out += tree_level.as_str();

            let mut writer_next = Arc::clone(writer);

            if entry.path().is_dir() {
                out += print_paths(&entry.path(), &recurisve_indent, &mut writer_next).as_str()
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
    use tempfile;

    #[test]
    fn test_print_paths() {
        let dir = create_directory_tree();
        let mut output: Arc<Mutex<Vec<u8>>> = Arc::new(Mutex::new(Vec::new()));
        let mut indent: Vec<TreeLevel> = Vec::new();

        println!("tmpdir: {:?}", dir);
        let out = print_paths(&dir.path(), &mut indent, &mut output);

        let unpacked_output = output.lock().unwrap();
        let output_string = String::from_utf8_lossy(&unpacked_output);
        println!("{}", output_string);
        // assert_eq!(output_string, expected_output_standard());
        assert_eq!(out, expected_output_standard());
    }

    #[test]
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

        let entries = read_dir_sorted(&dir);

        for (i, entry) in entries.iter().enumerate() {
            println!("{:?} -- {}", entry.file_name(), sorted_dirs[i]);
            assert_eq!(entry.file_name().into_string().unwrap(), sorted_dirs[i]);
        }
    }

    #[test]
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
