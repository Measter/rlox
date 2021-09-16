mod chunk;
mod value;

use color_eyre::Result;
use rlox::source_file::{SourceFile, SourceLocation};

use chunk::{Chunk, OpCode};
use value::Value;

fn main() -> Result<()> {
    color_eyre::install()?;

    let mut source_file = SourceFile::new();
    let id = source_file.add("script", "print 1.2; return;");

    let mut chunk = Chunk::default();
    let constant_id = chunk.add_constant(Value::Number(1.2));
    chunk.write(
        OpCode::Constant,
        SourceLocation {
            file_id: id,
            source_start: 6,
            len: 3,
        },
    );
    chunk.write(
        constant_id,
        SourceLocation {
            file_id: id,
            source_start: 6,
            len: 3,
        },
    );
    chunk.write(
        OpCode::Return,
        SourceLocation {
            file_id: id,
            source_start: 11,
            len: 6,
        },
    );

    chunk.disassemble(&source_file);

    Ok(())
}
