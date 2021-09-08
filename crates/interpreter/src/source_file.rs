// Basically a minor modification to codespan_reportings SimpleFiles because I wanted
// a more strongly-typed file ID.

use codespan_reporting::files::{Files, SimpleFile};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(usize);

impl FileId {
    pub fn blank() -> Self {
        Self(0)
    }
}

pub struct SourceFile {
    files: Vec<SimpleFile<String, String>>,
}

impl SourceFile {
    pub fn new() -> Self {
        SourceFile { files: Vec::new() }
    }

    pub fn add(&mut self, name: &str, source: &str) -> FileId {
        let id = self.files.len();
        self.files
            .push(SimpleFile::new(name.to_owned(), source.to_owned()));

        FileId(id)
    }
}

impl<'a> Files<'a> for SourceFile {
    type FileId = FileId;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok(self.files[id.0].name())
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        Ok(self.files[id.0].source().as_ref())
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.files[id.0].line_index((), byte_index)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        self.files[id.0].line_range((), line_index)
    }
}
