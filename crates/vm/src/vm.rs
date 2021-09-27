use core::panic;
use std::{
    convert::TryInto,
    fmt::Write,
    io::{StdoutLock, Write as _},
};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use color_eyre::Result;
use fnv::FnvHashMap as HashMap;
use lasso::{Rodeo, Spur};
use rlox::{
    source_file::{FileId, SourceLocation},
    DiagnosticEmitter,
};

use crate::{
    chunk::{Chunk, OpCode},
    value::{StringObject, Value},
};

#[derive(Clone)]
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
    globals: HashMap<Spur, RuntimeValue>,
    stdout: StdoutLock<'stdout>,
}

impl<'stdout> Vm<'stdout> {
    pub fn new(trace: bool, stdout: StdoutLock<'stdout>) -> Self {
        Self {
            trace,
            value_stack: Vec::new(),
            stdout,
            globals: HashMap::default(),
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
        interner: &Rodeo,
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

            // This bit feels hacky...
            (Value::String(b), Value::String(a)) if op == OpCode::Add => {
                let result = a.concatinate_strings(&b, interner);
                left.value = Value::String(result);
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
                let mut stderr = std::io::stderr();
                stderr.write_all(b"  [").unwrap();
                let mut vals = self.value_stack.iter();
                if let Some(val) = vals.next() {
                    val.value.dump(&mut stderr, interner);
                }
                for val in vals {
                    stderr.write_all(b", ").unwrap();
                    val.value.dump(&mut stderr, interner);
                }
                stderr.write_all(b"]\n").unwrap();
                chunk.disassemble_instruction(&mut stderr, emitter, idx, interner);
            }

            match current_op {
                OpCode::Add => self.binary_op(
                    |a, b| Value::Number(a + b),
                    current_op,
                    op_location,
                    interner,
                )?,
                OpCode::Divide => self.binary_op(
                    |a, b| Value::Number(a / b),
                    current_op,
                    op_location,
                    interner,
                )?,
                OpCode::Subtract => self.binary_op(
                    |a, b| Value::Number(a - b),
                    current_op,
                    op_location,
                    interner,
                )?,
                OpCode::Multiply => self.binary_op(
                    |a, b| Value::Number(a * b),
                    current_op,
                    op_location,
                    interner,
                )?,
                OpCode::Modulo => self.binary_op(
                    |a, b| Value::Number(a % b),
                    current_op,
                    op_location,
                    interner,
                )?,
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
                OpCode::DefineGlobal => {
                    let variable = self.read_constant(chunk, &mut ip).clone();
                    let name_id = match variable {
                        Value::String(StringObject::Literal(id)) => id,
                        _ => panic!("ICE: Global identifier wasn't a string literal"),
                    };

                    let value = self
                        .value_stack
                        .last()
                        .expect("ICE: DefineGlobal requires ID")
                        .clone();

                    self.globals.insert(name_id, value);
                    self.value_stack.pop();
                }
                OpCode::GetGlobal => {
                    let variable = self.read_constant(chunk, &mut ip).clone();
                    let name_id = match variable {
                        Value::String(StringObject::Literal(id)) => id,
                        _ => panic!("ICE: Global identifier wasn't a string literal"),
                    };

                    let value = match self.globals.get(&name_id) {
                        Some(v) => v,
                        None => {
                            let diag = Diagnostic::error()
                                .with_message("Undefined variable")
                                .with_labels(vec![Label::primary(file_id, op_location.range())]);
                            return Err(diag);
                        }
                    };

                    self.value_stack.push(value.clone());
                }
                OpCode::SetGlobal => {
                    let variable = self.read_constant(chunk, &mut ip).clone();
                    let name_id = match variable {
                        Value::String(StringObject::Literal(id)) => id,
                        _ => panic!("ICE: Global identifier wasn't a string literal"),
                    };

                    let value = self
                        .value_stack
                        .last()
                        .expect("ICE: DefineGlobal requires ID")
                        .clone();

                    match self.globals.get_mut(&name_id) {
                        Some(v) => *v = value,
                        None => {
                            let diag = Diagnostic::error()
                                .with_message("Undefined variable")
                                .with_labels(vec![Label::primary(file_id, op_location.range())]);
                            return Err(diag);
                        }
                    }
                }

                OpCode::Equal => {
                    let (right, left) = self
                        .value_stack
                        .pop()
                        .zip(self.value_stack.last_mut())
                        .expect("ICE Expected value on stack");
                    left.value = Value::Boolean(left.value.eq(&right.value, interner));
                    left.source = left.source.merge(right.source);
                }
                OpCode::Greater => self.binary_op(
                    |a, b| Value::Boolean(a > b),
                    current_op,
                    op_location,
                    interner,
                )?,
                OpCode::Less => self.binary_op(
                    |a, b| Value::Boolean(a < b),
                    current_op,
                    op_location,
                    interner,
                )?,

                OpCode::Pop => {
                    self.value_stack
                        .pop()
                        .expect("ICE: Expected value on stack");
                }
                OpCode::Print => {
                    let value = self
                        .value_stack
                        .pop()
                        .expect("ICE: Expected value on stack");
                    value.value.display(&mut self.stdout, interner);
                }
                OpCode::Return => return Ok(()),
            }
        }
    }
}
