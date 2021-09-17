use codespan_reporting::{
    diagnostic::Diagnostic,
    term::{
        termcolor::{StandardStream, StandardStreamLock},
        Config,
    },
};
use color_eyre::eyre::{Context, Result};

pub mod source_file;
use source_file::{FileId, SourceFile};
pub mod lexer;
pub mod token;

pub struct DiagnosticEmitter<'a> {
    cfg: Config,
    file: SourceFile,
    stderr: StandardStreamLock<'a>,
}

impl<'a> DiagnosticEmitter<'a> {
    pub fn new(output: &'a StandardStream) -> Self {
        DiagnosticEmitter {
            cfg: codespan_reporting::term::Config::default(),
            file: SourceFile::new(),
            stderr: output.lock(),
        }
    }

    pub fn add_file(&mut self, name: &str, contents: &str) -> FileId {
        self.file.add(name, contents)
    }

    pub fn sources(&self) -> &SourceFile {
        &self.file
    }

    pub fn emit_diagnostic(&mut self, diag: &Diagnostic<FileId>) -> Result<()> {
        codespan_reporting::term::emit(&mut self.stderr, &self.cfg, &self.file, diag)
            .with_context(|| "Failed to write diagnostic")?;

        Ok(())
    }

    pub fn emit_diagnostics(&mut self, diags: &[Diagnostic<FileId>]) -> Result<()> {
        diags.iter().try_for_each(|d| self.emit_diagnostic(d))?;

        Ok(())
    }
}
