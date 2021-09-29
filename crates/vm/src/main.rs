mod chunk;
mod compiler;
mod object;
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
use compiler::Compiler;
use lasso::Rodeo;
use rlox::{lexer::Lexer, source_file::FileId, DiagnosticEmitter};
use structopt::StructOpt;

use vm::Vm;

#[derive(Debug, StructOpt)]
struct Args {
    /// Print a disassembly of the next instruction to be executed along with the value stack.
    #[structopt(long)]
    trace: bool,

    /// Print a disassembly of each chunk before executing.
    #[structopt(long)]
    dump: bool,

    files: Vec<String>,
}

fn main() -> Result<()> {
    color_eyre::install()?;

    let args = Args::from_args();

    let stderr = StandardStream::stderr(ColorChoice::Always);
    let stdout = std::io::stdout();
    let mut emitter = DiagnosticEmitter::new(&stderr);
    let mut interner = Rodeo::default();
    // These three are used when instantiating a class.
    interner.get_or_intern_static("this");
    interner.get_or_intern_static("init");
    interner.get_or_intern_static("super");
    let mut vm = Vm::new(args.trace, stdout.lock());

    for file in &args.files {
        run_file(
            file,
            &mut emitter,
            &mut vm,
            &mut interner,
            args.dump,
            args.trace,
        )?;
    }

    Ok(())
}

fn run_file(
    path: &str,
    emitter: &mut DiagnosticEmitter<'_>,
    vm: &mut Vm,
    interner: &mut Rodeo,
    dump_chunk: bool,
    trace: bool,
) -> Result<()> {
    let contents =
        std::fs::read_to_string(path).with_context(|| eyre!("Failed to read file: '{}'", path))?;

    let file_id = emitter.add_file(path, &contents);

    if let Some(diags) = run(&contents, emitter, vm, interner, file_id, dump_chunk, trace)? {
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
    dump_chunk: bool,
    trace: bool,
) -> Result<Option<Vec<Diagnostic<FileId>>>> {
    let scan_result = Lexer::scan_tokens(source, interner, file_id);
    let tokens = match scan_result {
        Ok(tokens) => tokens,
        Err(diags) => return Ok(Some(diags)),
    };

    let chunk = match Compiler::compile(&tokens, emitter, interner) {
        Ok(chunk) => chunk,
        Err(diag) => return Ok(Some(diag)),
    };

    if dump_chunk {
        eprintln!("=== CHUNK ===");
        let mut stderr = std::io::stderr();
        chunk.disassemble(&mut stderr, emitter, interner);
    }

    if trace {
        if dump_chunk {
            eprintln!("\n");
        }

        eprintln!("=== Execution ===");
    }

    match vm.interpret(&chunk, emitter, file_id, interner) {
        Ok(_) => Ok(None),
        Err(diag) => Ok(Some(vec![diag])),
    }
}
