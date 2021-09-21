mod chunk;
mod compiler;
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

use vm::Vm;

fn main() -> Result<()> {
    color_eyre::install()?;

    let mut trace = false;
    let mut dump_chunk = false;

    for (key, _) in std::env::vars() {
        trace |= key == "LOX_TRACE";
        dump_chunk |= key == "LOX_DUMP";
    }

    let args: Vec<_> = std::env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("Usage: rloxum [scripts]");
        std::process::exit(64);
    }

    let stderr = StandardStream::stderr(ColorChoice::Always);
    let stdout = std::io::stdout();
    let mut emitter = DiagnosticEmitter::new(&stderr);
    let mut interner = Rodeo::default();
    // These three are used when instantiating a class.
    interner.get_or_intern_static("this");
    interner.get_or_intern_static("init");
    interner.get_or_intern_static("super");
    let mut vm = Vm::new(trace, stdout.lock());

    for file in args {
        run_file(
            &file,
            &mut emitter,
            &mut vm,
            &mut interner,
            dump_chunk,
            trace,
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
        Err(diag) => return Ok(Some(vec![diag])),
    };

    if dump_chunk {
        eprintln!("=== CHUNK ===");
        chunk.disassemble(emitter);
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
