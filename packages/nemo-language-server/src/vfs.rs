use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

pub struct Vfs {
    files: HashMap<PathBuf, String>,
}

impl Default for Vfs {
    fn default() -> Self {
        Self::new()
    }
}

impl Vfs {
    pub fn new() -> Vfs {
        Vfs {
            files: HashMap::new(),
        }
    }
    pub fn open_file(&mut self, path: PathBuf) -> io::Result<()> {
        // TODO error if file is already held?
        let content = fs::read_to_string(&path)?;
        self.files.insert(path, content);
        Ok(())
    }

    pub fn insert_file(&mut self, path: PathBuf, content: String) {
        self.files.insert(path, content);
    }

    pub fn read_file(&mut self, path: &Path) -> Option<&String> {
        self.files.get(path)
    }

    pub fn update_file(&mut self, path: &Path, content: String) {
        let entry = self
            .files
            .get_mut(path)
            .expect("Tried to update a non-existing file");
        *entry = content;
    }

    pub fn remove_file(&mut self, path: &Path) {
        self.files.remove(path);
    }

    pub fn list_files(&self) -> Vec<&Path> {
        self.files.keys().map(|p| p.as_path()).collect()
    }
}
