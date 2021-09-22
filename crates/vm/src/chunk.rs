use std::io::Write;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::Files,
};
use lasso::Rodeo;
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
    False,
    Equal,
    Greater,
    Less,
    Negate,
    Multiply,
    Modulo,
    Nil,
    Not,
    Return,
    Subtract,
    True,
}

impl OpCode {
    fn len(self) -> usize {
        use OpCode::*;
        match self {
            Constant => 2,
            Add | Divide | Multiply | Modulo | Negate | Not | Subtract => 1,
            Return => 1,
            False | True | Nil => 1,
            Equal | Greater | Less => 1,
        }
    }

    pub fn binary_operator_str(self) -> &'static str {
        match self {
            OpCode::Add => "+",
            OpCode::Divide => "/",
            OpCode::Subtract | OpCode::Negate => "-",
            OpCode::Multiply => "*",
            OpCode::Modulo => "%",
            _ => "",
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

    pub fn disassemble(
        &self,
        stream: &mut impl Write,
        emitter: &DiagnosticEmitter<'_>,
        interner: &Rodeo,
    ) {
        let mut offset = 0;
        while offset < self.code.len() {
            offset += self.disassemble_instruction(stream, emitter, offset, interner);
        }
    }

    pub fn disassemble_instruction(
        &self,
        stream: &mut impl Write,
        emitter: &DiagnosticEmitter<'_>,
        offset: usize,
        interner: &Rodeo,
    ) -> usize {
        write!(stream, "0x{:04X} ", offset).unwrap();
        let cur_loc = self.locations[offset];
        let prev_loc = (offset > 0).then(|| self.locations[offset - 1]);

        if Some(cur_loc) == prev_loc {
            write!(stream, "{:24}| ", "").unwrap();
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

            write!(stream, "{:<25} ", formatted).unwrap();
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
                write!(stream, "{:<16?} {} '", OpCode::Constant, constant_id,).unwrap();
                constant_value.display(stream, interner, false);
                stream.write_all(b"'\n").unwrap();
                OpCode::Constant.len()
            }
            Ok(other) => {
                writeln!(stream, "{:?}", other).unwrap();
                other.len()
            }
            Err(_) => {
                writeln!(stream, "0x{:02X}", op).unwrap();
                1
            }
        }
    }
}
