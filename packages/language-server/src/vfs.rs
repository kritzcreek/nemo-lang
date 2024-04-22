use frontend::run_frontend;
use frontend::types::CheckResult;
use frontend::CheckError;
use line_index::LineIndex;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct FileData {
    pub content: String,
    pub check_result: CheckResult<CheckError>,
    pub line_index: LineIndex,
}

pub struct Vfs {
    files: HashMap<PathBuf, FileData>,
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
        let check_result = run_frontend(&content);
        let line_index = LineIndex::new(&content);
        self.files.insert(
            path,
            FileData {
                content,
                check_result,
                line_index,
            },
        );
        Ok(())
    }

    pub fn insert_file(&mut self, path: &Path, content: String) -> &FileData {
        let check_result = run_frontend(&content);
        let line_index = LineIndex::new(&content);
        self.files.insert(
            path.to_owned(),
            FileData {
                content,
                check_result,
                line_index,
            },
        );
        self.files.get(path).unwrap()
    }

    pub fn read_file(&self, path: &Path) -> Option<&FileData> {
        self.files.get(path)
    }

    pub fn update_file(&mut self, path: &Path, content: String) -> &FileData {
        let check_result = run_frontend(&content);
        let line_index = LineIndex::new(&content);
        self.files.insert(
            path.to_owned(),
            FileData {
                content,
                check_result,
                line_index,
            },
        );
        self.files.get(path).unwrap()
    }

    pub fn remove_file(&mut self, path: &Path) {
        self.files.remove(path);
    }

    pub fn list_files(&self) -> Vec<&Path> {
        self.files.keys().map(|p| p.as_path()).collect()
    }
}
