use std::convert::TryInto;

use codespan_reporting::files::Files;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use rlox::source_file::{SourceFile, SourceLocation};

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Return,
}

impl OpCode {
    fn len(self) -> usize {
        match self {
            OpCode::Return => 1,
            OpCode::Constant => 2,
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
    code: Vec<u8>,
    constants: Vec<Value>,
    locations: Vec<SourceLocation>,
}

impl Chunk {
    pub fn write(&mut self, data: impl Into<u8>, location: SourceLocation) {
        self.code.push(data.into());
        self.locations.push(location);
    }

    pub fn add_constant(&mut self, value: Value) -> ConstId {
        self.constants.push(value);
        ConstId(
            (self.constants.len() - 1)
                .try_into()
                .expect("ICE: Too many constants"),
        )
    }

    pub fn disassemble(&self, sources: &SourceFile) {
        let mut offset = 0;
        let prev_loc = None;
        while offset < self.code.len() {
            print!("0x{:04X} ", offset);
            let cur_loc = self.locations[offset];

            if Some(cur_loc) == prev_loc {
                print!("{:24}| ", "");
            } else {
                let source_location = sources
                    .location(cur_loc.file_id, cur_loc.source_start)
                    .expect("ICE: Invalid file ID/byte index");
                let source_name = sources.name(cur_loc.file_id).expect("ICE: Invalid file ID");
                let formatted = format!(
                    "<{}:{}:{}>",
                    source_name, source_location.line_number, source_location.column_number
                );

                print!("{:<25} ", formatted);
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
                    println!(
                        "{:<16} {} '{:?}'",
                        "OP_CONSTANT", constant_id, constant_value
                    );
                    offset += OpCode::Constant.len();
                }
                Ok(OpCode::Return) => {
                    println!("OP_RETURN");
                    offset += OpCode::Return.len();
                }
                Err(_) => {
                    println!("0x{:02X}", op);
                    offset += 1;
                }
            }
        }
    }
}
