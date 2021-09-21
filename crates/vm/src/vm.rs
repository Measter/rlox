use core::panic;
use std::{convert::TryInto, fmt::Write, io::StdoutLock};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use color_eyre::Result;
use lasso::Rodeo;
use rlox::{
    source_file::{FileId, SourceLocation},
    DiagnosticEmitter,
};

use crate::{
    chunk::{Chunk, OpCode},
    value::Value,
};

struct RuntimeValue {
    value: Value,
    source: SourceLocation,
}

impl std::fmt::Debug for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

pub struct Vm<'stdout> {
    trace: bool,
    value_stack: Vec<RuntimeValue>,
    stdout: StdoutLock<'stdout>,
}

impl<'stdout> Vm<'stdout> {
    pub fn new(trace: bool, stdout: StdoutLock<'stdout>) -> Self {
        Self {
            trace,
            value_stack: Vec::new(),
            stdout,
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

    fn binary_op(
        &mut self,
        func: fn(f64, f64) -> Value,
        op: OpCode,
        location: SourceLocation,
    ) -> Result<(), Diagnostic<FileId>> {
        let (right, left) = self
            .value_stack
            .pop()
            .zip(self.value_stack.last_mut())
            .expect("ICE: Value stack empty");

        match (right.value, &left.value) {
            (Value::Number(b), Value::Number(a)) => {
                left.value = func(*a, b);
                left.source = left.source.merge(right.source);
                Ok(())
            }

            (right_value, left_value) => {
                let mut msg = format!(
                    "unable to apply operator `{}` to {}",
                    op.binary_operator_str(),
                    left_value.kind()
                );

                if left_value.kind() != right_value.kind() {
                    write!(&mut msg, " and {}", right_value.kind()).unwrap();
                }

                let labels = vec![
                    Label::primary(location.file_id, location.range()),
                    Label::secondary(left.source.file_id, left.source.range())
                        .with_message(left_value.kind()),
                    Label::secondary(right.source.file_id, right.source.range())
                        .with_message(right_value.kind()),
                ];

                Err(Diagnostic::error().with_message(msg).with_labels(labels))
            }
        }
    }

    fn unary_op(&mut self, op: OpCode, location: SourceLocation) -> Result<(), Diagnostic<FileId>> {
        let value = self.value_stack.last_mut().expect("ICE: Value stack empty");

        match (op, &mut value.value) {
            (OpCode::Negate, Value::Number(num)) => {
                *num = -*num;
                value.source = value.source.merge(location);
            }
            (OpCode::Negate, v) => {
                return Err(Diagnostic::error()
                    .with_message(format!("unable to negate a {}", v.kind()))
                    .with_labels(vec![Label::primary(location.file_id, location.range())]))
            }
            (OpCode::Not, v) => {
                *v = Value::Boolean(v.is_falsy());
                value.source = value.source.merge(location);
            }

            _ => panic!("ICE: unary_op called with non-unary operator"),
        }

        Ok(())
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
            let (current_op, idx) = ip
                .next()
                .map(|(op, idx)| op.try_into().map(|op| (op, idx)))
                .expect("ICE: Unexpected end of instructions")
                .expect("ICE: Invalid opcode");

            let op_location = chunk.locations[idx];

            if self.trace {
                eprintln!("  {:?}", self.value_stack);
                chunk.disassemble_instruction(emitter, idx);
            }

            match current_op {
                OpCode::Add => {
                    self.binary_op(|a, b| Value::Number(a + b), current_op, op_location)?
                }
                OpCode::Divide => {
                    self.binary_op(|a, b| Value::Number(a / b), current_op, op_location)?
                }
                OpCode::Subtract => {
                    self.binary_op(|a, b| Value::Number(a - b), current_op, op_location)?
                }
                OpCode::Multiply => {
                    self.binary_op(|a, b| Value::Number(a * b), current_op, op_location)?
                }
                OpCode::Modulo => {
                    self.binary_op(|a, b| Value::Number(a % b), current_op, op_location)?
                }
                OpCode::Negate => self.unary_op(current_op, op_location)?,
                OpCode::Not => self.unary_op(current_op, op_location)?,

                OpCode::Nil => self.value_stack.push(RuntimeValue {
                    value: Value::Nil,
                    source: op_location,
                }),
                OpCode::False => self.value_stack.push(RuntimeValue {
                    value: Value::Boolean(false),
                    source: op_location,
                }),
                OpCode::True => self.value_stack.push(RuntimeValue {
                    value: Value::Boolean(true),
                    source: op_location,
                }),
                OpCode::Constant => {
                    let value = self.read_constant(chunk, &mut ip).clone();
                    self.value_stack.push(RuntimeValue {
                        value,
                        source: op_location,
                    });
                }

                OpCode::Equal => {
                    let (right, left) = self
                        .value_stack
                        .pop()
                        .zip(self.value_stack.last_mut())
                        .expect("ICE Expected value on stack");
                    left.value = Value::Boolean(left.value == right.value);
                    left.source = left.source.merge(right.source);
                }
                OpCode::Greater => {
                    self.binary_op(|a, b| Value::Boolean(a > b), current_op, op_location)?
                }
                OpCode::Less => {
                    self.binary_op(|a, b| Value::Boolean(a < b), current_op, op_location)?
                }

                OpCode::Return => {
                    let value = self
                        .value_stack
                        .pop()
                        .expect("ICE: Expected value on stack");
                    value.value.display(&mut self.stdout);
                    return Ok(());
                }
            }
        }
    }
}
