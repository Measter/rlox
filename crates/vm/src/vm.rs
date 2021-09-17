use core::panic;
use std::{
    convert::TryInto,
    ops::{Add, Div, Mul, Rem, Sub},
};

use codespan_reporting::diagnostic::Diagnostic;
use color_eyre::Result;
use lasso::Rodeo;
use rlox::{source_file::FileId, DiagnosticEmitter};

use crate::{
    chunk::{Chunk, OpCode},
    value::Value,
};

pub struct Vm {
    trace: bool,
    value_stack: Vec<Value>,
}

impl Vm {
    pub fn new(trace: bool) -> Self {
        Self {
            trace,
            value_stack: Vec::new(),
        }
    }

    fn read_constant<'a>(
        &mut self,
        chunk: &'a Chunk,
        ip: &mut impl Iterator<Item = (u8, usize)>,
    ) -> &'a Value {
        let (idx, _) = ip.next().expect("ICE: expected constant ID");

        chunk
            .constants
            .get(idx as usize)
            .expect("ICE: invalid constant ID")
    }

    fn binary_op(&mut self, func: fn(f64, f64) -> f64) {
        let vals = self.value_stack.pop().zip(self.value_stack.last_mut());
        match vals {
            Some((Value::Number(b), Value::Number(a))) => {
                *a = func(*a, b);
            }
            None => panic!("ICE: Value stack empty"),
        }
    }

    pub fn interpret(
        &mut self,
        chunk: &Chunk,
        emitter: &DiagnosticEmitter<'_>,
        file_id: FileId,
        interner: &Rodeo,
    ) -> Result<(), Diagnostic<FileId>> {
        let mut ip = chunk.code.iter().copied().zip(0..);
        loop {
            let (next_instruction, idx) = ip
                .next()
                .map(|(op, idx)| op.try_into().map(|op| (op, idx)))
                .expect("ICE: Unexpected end of instructions")
                .expect("ICE: Invalid opcode");

            if self.trace {
                eprintln!("{:?}", self.value_stack);
                chunk.disassemble_instruction(emitter, idx);
            }

            match next_instruction {
                OpCode::Add => self.binary_op(Add::add),
                OpCode::Divide => self.binary_op(Div::div),
                OpCode::Subtract => self.binary_op(Sub::sub),
                OpCode::Multiply => self.binary_op(Mul::mul),
                OpCode::Modulo => self.binary_op(Rem::rem),
                OpCode::Negate => {
                    match self.value_stack.last_mut() {
                        Some(Value::Number(num)) => *num = -*num,
                        _ => panic!("ICE: Expected number"),
                    };
                }
                OpCode::Constant => {
                    let value = self.read_constant(chunk, &mut ip).clone();
                    self.value_stack.push(value);
                }
                OpCode::Return => {
                    let value = self
                        .value_stack
                        .pop()
                        .expect("ICE: Expected value on stack");
                    value.display();
                    return Ok(());
                }
            }
        }
    }
}
