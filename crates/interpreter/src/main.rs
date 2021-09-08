#![warn(rust_2018_idioms)]

use codespan_reporting::{
    diagnostic::Diagnostic,
    term::{
        termcolor::{ColorChoice, StandardStream, StandardStreamLock},
        Config,
    },
};
use color_eyre::{eyre::eyre, eyre::Context, Result};
use lasso::Rodeo;
use resolver::Resolver;
use source_file::{FileId, SourceFile};

mod ast;
mod environment;
mod interpreter;
mod lexer;
mod lox_callable;
mod parser;
mod resolver;
mod source_file;
mod token;

use crate::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

pub struct DiagnosticEmitter<'a> {
    cfg: Config,
    file: SourceFile,
    stderr: StandardStreamLock<'a>,
}

impl<'a> DiagnosticEmitter<'a> {
    fn new(output: &'a StandardStream) -> Self {
        DiagnosticEmitter {
            cfg: codespan_reporting::term::Config::default(),
            file: SourceFile::new(),
            stderr: output.lock(),
        }
    }

    fn add_file(&mut self, name: &str, contents: &str) -> FileId {
        self.file.add(name, contents)
    }

    fn emit_diagnostic(&mut self, diag: &Diagnostic<FileId>) -> Result<()> {
        codespan_reporting::term::emit(&mut self.stderr, &self.cfg, &self.file, diag)
            .with_context(|| "Failed to write diagnostic")?;

        Ok(())
    }
    fn emit_diagnostics(&mut self, diags: &[Diagnostic<FileId>]) -> Result<()> {
        diags.iter().try_for_each(|d| self.emit_diagnostic(d))?;

        Ok(())
    }
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args: Vec<_> = std::env::args().skip(1).collect();

    if args.is_empty() {
        eprintln!("Usage: rloxr [script]");
        std::process::exit(64);
    }

    let stderr = StandardStream::stderr(ColorChoice::Always);
    let mut emitter = DiagnosticEmitter::new(&stderr);

    let mut interner = Rodeo::default();
    // These two are used when instantiating a class.
    interner.get_or_intern_static("this");
    interner.get_or_intern_static("init");
    let mut interpreter = Interpreter::new(&mut interner);

    for file in args {
        run_file(&file, &mut emitter, &mut interpreter, &mut interner)?;
    }

    Ok(())
}

fn run_file(
    path: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    interpreter: &mut Interpreter,
    interner: &mut Rodeo,
) -> Result<()> {
    let contents =
        std::fs::read_to_string(path).with_context(|| eyre!("Failed to read file: '{}'", path))?;

    let file_id = emitter.add_file(path, &contents);

    if let Some(diags) = run(&contents, emitter, interpreter, interner, file_id)? {
        emitter.emit_diagnostics(&diags)?;

        std::process::exit(64);
    }

    Ok(())
}

fn run(
    source: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    interpreter: &mut Interpreter,
    interner: &mut Rodeo,
    file_id: FileId,
) -> Result<Option<Vec<Diagnostic<FileId>>>> {
    let scan_result = Lexer::scan_tokens(source, interner, file_id);

    let tokens = match scan_result {
        Ok(tokens) => tokens,
        Err(diags) => return Ok(Some(diags)),
    };

    let program = match Parser::parse(&tokens, interner, file_id) {
        Ok(program) => program,
        Err(diags) => return Ok(Some(diags)),
    };

    emitter.emit_diagnostics(&program.diags)?;

    let mut resolver = Resolver::new(interpreter, file_id, interner);
    if let Err(diag) = resolver.resolve(&program.program) {
        return Ok(Some(vec![diag]));
    }

    if let Err(diag) = interpreter.interpret(&program.program, emitter, interner) {
        return Ok(Some(vec![diag]));
    }

    if interpreter.did_print() {
        println!();
    }

    Ok(None)
}
