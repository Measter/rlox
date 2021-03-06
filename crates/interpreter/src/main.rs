#![warn(rust_2018_idioms)]

use codespan_reporting::{
    diagnostic::Diagnostic,
    term::termcolor::{ColorChoice, StandardStream},
};
use color_eyre::{eyre::eyre, eyre::Context, Result};
use lasso::Rodeo;
use rlox::{
    lexer::Lexer, parser::Parser, program::Program, resolver::Resolver, source_file::FileId,
    DiagnosticEmitter,
};

mod environment;
mod interpreter;
mod lox_callable;

use crate::interpreter::Interpreter;

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
    let this_lexeme = interner.get_or_intern_static("this");
    let init_lexeme = interner.get_or_intern_static("init");
    let super_lexeme = interner.get_or_intern_static("super");
    let mut program = Program::new(this_lexeme, init_lexeme, super_lexeme);
    let mut interpreter = Interpreter::new(&mut interner);

    for file in args {
        run_file(
            &file,
            &mut emitter,
            &mut interpreter,
            &mut interner,
            &mut program,
        )?;
    }

    Ok(())
}

fn run_file(
    path: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    interpreter: &mut Interpreter,
    interner: &mut Rodeo,
    program: &mut Program,
) -> Result<()> {
    let contents =
        std::fs::read_to_string(path).with_context(|| eyre!("Failed to read file: '{}'", path))?;

    let file_id = emitter.add_file(path, &contents);

    if let Some(diags) = run(&contents, emitter, interpreter, interner, file_id, program)? {
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
    program: &mut Program,
) -> Result<Option<Vec<Diagnostic<FileId>>>> {
    let scan_result = Lexer::scan_tokens(source, interner, file_id);

    let tokens = match scan_result {
        Ok(tokens) => tokens,
        Err(diags) => return Ok(Some(diags)),
    };

    let parsed_program = match Parser::parse(&tokens, interner, file_id, program) {
        Ok(program) => program,
        Err(diags) => return Ok(Some(diags)),
    };

    emitter.emit_diagnostics(&parsed_program.diags)?;

    if let Err(diag) = Resolver::resolve(file_id, interner, program, &parsed_program.program) {
        return Ok(Some(vec![diag]));
    }

    if let Err(diag) = interpreter.interpret(&parsed_program.program, emitter, interner, program) {
        return Ok(Some(vec![diag]));
    }

    if interpreter.did_print() {
        println!();
    }

    Ok(None)
}
