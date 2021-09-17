use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::Files,
};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use rlox::{
    source_file::{FileId, SourceLocation},
    DiagnosticEmitter,
};

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Add,
    Constant,
    Divide,
    Negate,
    Multiply,
    Modulo,
    Return,
    Subtract,
}

impl OpCode {
    fn len(self) -> usize {
        use OpCode::*;
        match self {
            Constant => 2,
            Add | Divide | Negate | Multiply | Modulo | Subtract => 1,
            Return => 1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstId(u8);

impl From<ConstId> for u8 {
    fn from(id: ConstId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone, Default)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub locations: Vec<SourceLocation>,
}

impl Chunk {
    pub const fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            locations: Vec::new(),
        }
    }

    pub fn write(&mut self, data: impl Into<u8>, location: SourceLocation) {
        self.code.push(data.into());
        self.locations.push(location);
    }

    pub fn add_constant(
        &mut self,
        value: Value,
        location: SourceLocation,
    ) -> Result<ConstId, Diagnostic<FileId>> {
        self.constants.push(value);

        let id = self.constants.len() - 1;

        if id < 256 {
            Ok(ConstId(id as u8))
        } else {
            let diag = Diagnostic::error()
                .with_message("too many constants in one chunk")
                .with_notes(vec!["up to 256 constants are allowed".to_owned()])
                .with_labels(vec![Label::primary(location.file_id, location.range())]);

            Err(diag)
        }
    }

    pub fn disassemble(&self, emitter: &DiagnosticEmitter<'_>) {
        let mut offset = 0;
        while offset < self.code.len() {
            offset += self.disassemble_instruction(emitter, offset);
        }
    }

    pub fn disassemble_instruction(&self, emitter: &DiagnosticEmitter<'_>, offset: usize) -> usize {
        eprint!("0x{:04X} ", offset);
        let cur_loc = self.locations[offset];
        let prev_loc = (offset > 0).then(|| self.locations[offset - 1]);

        if Some(cur_loc) == prev_loc {
            eprint!("{:24}| ", "");
        } else {
            let source_location = emitter
                .sources()
                .location(cur_loc.file_id, cur_loc.source_start)
                .expect("ICE: Invalid file ID/byte index");
            let source_name = emitter
                .sources()
                .name(cur_loc.file_id)
                .expect("ICE: Invalid file ID");
            let formatted = format!(
                "<{}:{}:{}>",
                source_name, source_location.line_number, source_location.column_number
            );

            eprint!("{:<25} ", formatted);
        }
        let op = self.code[offset];
        match OpCode::try_from_primitive(op) {
            Ok(OpCode::Constant) => {
                let constant_id = *self
                    .code
                    .get(offset + 1)
                    .expect("ICE: Ran out of data while fetching constant");
                let constant_value = self
                    .constants
                    .get(constant_id as usize)
                    .expect("ICE: Invalid constant ID");
                eprintln!(
                    "{:<16?} {} '{:?}'",
                    OpCode::Constant,
                    constant_id,
                    constant_value
                );
                OpCode::Constant.len()
            }
            Ok(other) => {
                eprintln!("{:?}", other);
                other.len()
            }
            Err(_) => {
                eprintln!("0x{:02X}", op);
                1
            }
        }
    }
}
