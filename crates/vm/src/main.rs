mod chunk;
mod value;
mod vm;

use codespan_reporting::{
    diagnostic::Diagnostic,
    term::termcolor::{ColorChoice, StandardStream},
};
use color_eyre::{
    eyre::{eyre, Context},
    Result,
};
use lasso::Rodeo;
use rlox::{
    lexer::Lexer,
    source_file::{FileId, SourceFile, SourceLocation},
    DiagnosticEmitter,
};

use chunk::{Chunk, OpCode};
use value::Value;
use vm::Vm;

fn main() -> Result<()> {
    color_eyre::install()?;

    let trace = std::env::vars()
        .filter(|(key, _)| key == "LOX_TRACE")
        .count()
        > 0;

    let args: Vec<_> = std::env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("Usage: rloxum [scripts]");
        std::process::exit(64);
    }

    let stderr = StandardStream::stderr(ColorChoice::Always);
    let mut emitter = DiagnosticEmitter::new(&stderr);
    let mut interner = Rodeo::default();
    // These three are used when instantiating a class.
    interner.get_or_intern_static("this");
    interner.get_or_intern_static("init");
    interner.get_or_intern_static("super");
    let mut vm = Vm::new(trace);

    for file in args {
        run_file(&file, &mut emitter, &mut vm, &mut interner)?;
    }

    Ok(())
}

fn run_file(
    path: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    vm: &mut Vm,
    interner: &mut Rodeo,
) -> Result<()> {
    let contents =
        std::fs::read_to_string(path).with_context(|| eyre!("Failed to read file: '{}'", path))?;

    let file_id = emitter.add_file(path, &contents);

    if let Some(diags) = run(&contents, emitter, vm, interner, file_id)? {
        emitter.emit_diagnostics(&diags)?;
        std::process::exit(64);
    }

    Ok(())
}

fn run(
    source: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    vm: &mut Vm,
    interner: &mut Rodeo,
    file_id: FileId,
) -> Result<Option<Vec<Diagnostic<FileId>>>> {
    let scan_result = Lexer::scan_tokens(source, interner, file_id);
    let tokens = match scan_result {
        Ok(tokens) => tokens,
        Err(diags) => return Ok(Some(diags)),
    };

    Ok(None)
}
