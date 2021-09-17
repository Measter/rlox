mod chunk;
mod value;
mod vm;

use color_eyre::Result;
use rlox::source_file::{SourceFile, SourceLocation};

use chunk::{Chunk, OpCode};
use value::Value;
use vm::Vm;

fn main() -> Result<()> {
    color_eyre::install()?;

    let trace = std::env::vars()
        .filter(|(key, _)| key == "LOX_TRACE")
        .count()
        > 0;
    let mut vm = Vm::new(trace);

    let mut source_file = SourceFile::new();
    let id = source_file.add("script.lox", "return 1.2 + 3.4;");

    let mut chunk = Chunk::default();
    let a_id = chunk.add_constant(Value::Number(1.2));
    let b_id = chunk.add_constant(Value::Number(3.4));
    chunk.write(
        OpCode::Constant,
        SourceLocation {
            file_id: id,
            source_start: 7,
            len: 3,
        },
    );
    chunk.write(
        a_id,
        SourceLocation {
            file_id: id,
            source_start: 7,
            len: 3,
        },
    );
    chunk.write(
        OpCode::Constant,
        SourceLocation {
            file_id: id,
            source_start: 14,
            len: 3,
        },
    );
    chunk.write(
        b_id,
        SourceLocation {
            file_id: id,
            source_start: 14,
            len: 3,
        },
    );
    chunk.write(
        OpCode::Add,
        SourceLocation {
            file_id: id,
            source_start: 12,
            len: 1,
        },
    );
    chunk.write(
        OpCode::Return,
        SourceLocation {
            file_id: id,
            source_start: 0,
            len: 6,
        },
    );

    if trace {
        eprintln!("Chunk trace");
        chunk.disassemble(&source_file);
    }

    eprintln!("\nExecution");
    vm.interpret(&chunk, &source_file)?;

    Ok(())
}
