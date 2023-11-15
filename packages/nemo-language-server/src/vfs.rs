use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

pub struct Vfs {
    files: HashMap<PathBuf, String>,
}

impl Vfs {
    pub fn new() -> Vfs {
        Vfs {
            files: HashMap::new(),
        }
    }
    pub fn open_file<'a>(&'a mut self, path: PathBuf) -> io::Result<()> {
        // TODO error if file is already held?
        let content = fs::read_to_string(&path)?;
        self.files.insert(path, content);
        Ok(())
    }

    pub fn insert_file<'a>(&'a mut self, path: PathBuf, content: String) {
        self.files.insert(path, content);
    }

    pub fn read_file(&mut self, path: &Path) -> Option<&String> {
        self.files.get(path)
    }

    pub fn update_file<'a, 'b>(&'a mut self, path: &'b Path, content: String) {
        let entry = self
            .files
            .get_mut(path)
            .expect("Tried to update a non-existing file");
        *entry = content;
    }

    pub fn remove_file<'a, 'b>(&'a mut self, path: &'b Path) {
        self.files.remove(path);
    }

    pub fn list_files<'a>(&'a self) -> Vec<&'a Path> {
        self.files.keys().map(|p| p.as_path()).collect()
    }
}
