use line_index::LineIndex;
use nemo_parser::check_program;
use nemo_parser::types::NameMap;
use nemo_parser::CheckError;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct FileData {
    pub content: String,
    pub name_map: NameMap,
    pub diagnostics: Vec<CheckError>,
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
        let (name_map, diagnostics) = check_program(&content);
        let line_index = LineIndex::new(&content);
        self.files.insert(
            path,
            FileData {
                content,
                name_map,
                diagnostics,
                line_index,
            },
        );
        Ok(())
    }

    pub fn insert_file(&mut self, path: PathBuf, content: String) -> &FileData {
        let (name_map, diagnostics) = check_program(&content);
        let line_index = LineIndex::new(&content);
        self.files.entry(path).or_insert(FileData {
            content,
            name_map,
            diagnostics,
            line_index
        })
    }

    pub fn read_file(&self, path: &Path) -> Option<&FileData> {
        self.files.get(path)
    }

    pub fn update_file(&mut self, path: &Path, content: String) -> &FileData {
        let (name_map, diagnostics) = check_program(&content);
        let line_index = LineIndex::new(&content);
        self.files.insert(path.to_owned(), FileData {
            content,
            name_map,
            diagnostics,
            line_index
        });
        self.files.get(path).unwrap()
    }

    pub fn diagnostics(&self) -> Vec<(&Path, &NameMap, &[CheckError])> {
        self.files
            .iter()
            .map(|(path, file)| (path.as_path(), &file.name_map, file.diagnostics.as_slice()))
            .collect()
    }

    pub fn remove_file(&mut self, path: &Path) {
        self.files.remove(path);
    }

    pub fn list_files(&self) -> Vec<&Path> {
        self.files.keys().map(|p| p.as_path()).collect()
    }
}
