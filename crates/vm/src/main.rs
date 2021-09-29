mod ast_compiler;
mod chunk;
mod object;
mod token_compiler;
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
    lexer::Lexer, parser::Parser, program::Program, resolver::Resolver, source_file::FileId,
    DiagnosticEmitter,
};
use structopt::StructOpt;
use token_compiler::TokenCompiler;

use vm::Vm;

use crate::ast_compiler::AstCompiler;

#[derive(Debug, StructOpt)]
struct Args {
    /// Print a disassembly of the next instruction to be executed along with the value stack.
    #[structopt(long)]
    trace: bool,

    /// Print a disassembly of each chunk before executing.
    #[structopt(long)]
    dump: bool,

    /// Use the AST compiler.
    #[structopt(long = "ast")]
    ast_compiler: bool,

    files: Vec<String>,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Args::from_args();

    let stderr = StandardStream::stderr(ColorChoice::Always);
    let stdout = std::io::stdout();
    let mut emitter = DiagnosticEmitter::new(&stderr);

    let mut interner = Rodeo::default();
    let this_lexeme = interner.get_or_intern_static("this");
    let init_lexeme = interner.get_or_intern_static("init");
    let super_lexeme = interner.get_or_intern_static("super");
    interner.get_or_intern_static("nil");

    let mut program = Program::new(this_lexeme, init_lexeme, super_lexeme);

    let mut vm = Vm::new(args.trace, stdout.lock());

    for file in &args.files {
        run_file(
            &args,
            file,
            &mut emitter,
            &mut vm,
            &mut interner,
            &mut program,
        )?;
    }

    Ok(())
}

fn run_file(
    args: &Args,
    path: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    vm: &mut Vm,
    interner: &mut Rodeo,
    program: &mut Program,
) -> Result<()> {
    let contents =
        std::fs::read_to_string(path).with_context(|| eyre!("Failed to read file: '{}'", path))?;

    let file_id = emitter.add_file(path, &contents);

    if let Some(diags) = run(args, &contents, emitter, vm, interner, program, file_id)? {
        emitter.emit_diagnostics(&diags)?;
        std::process::exit(64);
    }

    Ok(())
}

fn run(
    args: &Args,
    source: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    vm: &mut Vm,
    interner: &mut Rodeo,
    program: &mut Program,
    file_id: FileId,
) -> Result<Option<Vec<Diagnostic<FileId>>>> {
    let scan_result = Lexer::scan_tokens(source, interner, file_id);
    let tokens = match scan_result {
        Ok(tokens) => tokens,
        Err(diags) => return Ok(Some(diags)),
    };

    let chunk = if args.ast_compiler {
        let parsed_program = match Parser::parse(&tokens, interner, file_id, program) {
            Ok(program) => program,
            Err(diags) => return Ok(Some(diags)),
        };

        emitter.emit_diagnostics(&parsed_program.diags)?;

        if let Err(diag) = Resolver::resolve(file_id, interner, program, &parsed_program.program) {
            return Ok(Some(vec![diag]));
        }

        match AstCompiler::compile(&parsed_program.program, interner, program, file_id) {
            Ok(chunk) => chunk,
            Err(diag) => return Ok(Some(vec![diag])),
        }
    } else {
        match TokenCompiler::compile(&tokens, emitter, interner) {
            Ok(chunk) => chunk,
            Err(diag) => return Ok(Some(diag)),
        }
    };

    if args.dump {
        eprintln!("=== CHUNK ===");
        let mut stderr = std::io::stderr();
        chunk.disassemble(&mut stderr, emitter, interner);
    }

    if args.trace {
        if args.dump {
            eprintln!("\n");
        }

        eprintln!("=== Execution ===");
    }

    match vm.interpret(&chunk, emitter, file_id, interner) {
        Ok(_) => Ok(None),
        Err(diag) => Ok(Some(vec![diag])),
    }
}
