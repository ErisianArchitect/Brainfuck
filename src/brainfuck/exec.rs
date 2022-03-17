use std::fs::DirBuilder;

use crate::brainfuck;
use brainfuck::io::{Utf8Writer,BufferedReader, BrainfuckReader, BrainfuckWriter};
use brainfuck::mem::{VirtualMemory, BrainfuckMemory};
use brainfuck::parser::{self, Instruction, Direction, OpCode, SyntaxError};

pub trait BrainfuckInterpreter {
    fn exec(&mut self, source: &str) -> Result<(), SyntaxError>;
    fn reset(&mut self) {}
}

pub struct Interpreter<RT: BrainfuckReader, WT: BrainfuckWriter, MT: BrainfuckMemory> {
    pub reader: RT,
    pub writer: WT,
    pub memory: MT,
}

pub type BasicInterpreter = Interpreter<BufferedReader, Utf8Writer, VirtualMemory>;

impl<RT: BrainfuckReader, WT: BrainfuckWriter, MT: BrainfuckMemory> BrainfuckInterpreter for Interpreter<RT,WT,MT> {
    fn exec(&mut self, source: &str) -> Result<(), SyntaxError> {
        let program = parser::load(source)?;
        let mut exec_idx = 0_usize;
        let mut address = 0_usize;
        while exec_idx < program.len() {
            match program[exec_idx] {
                Instruction::Add(sum) => {
                    let v = self.memory.get(address);
                    self.memory.set(address, v.wrapping_add(sum));
                    exec_idx += 1;
                }
                Instruction::Assign(value) => {
                    self.memory.set(address, value);
                    exec_idx += 1;
                }
                Instruction::Reset => {
                    self.memory.set(address, 0);
                    exec_idx += 1;
                }
                Instruction::EndRead => {
                    let mut current = self.reader.read_or(0);
                    while current != 0 {
                        self.memory.set(address, current);
                        current = self.reader.read_or(0);
                    }
                    exec_idx += 1;
                }
                Instruction::JumpZ(addr) => {
                    if self.memory.get(address) == 0 {
                        exec_idx = addr as usize;
                    } else {
                        exec_idx += 1;
                    }
                }
                Instruction::ReturnIf(addr) => {
                    if self.memory.get(address) != 0 {
                        exec_idx = addr as usize;
                    } else {
                        exec_idx += 1;
                    }
                }
                Instruction::Move(count) => { 
                    address = address.wrapping_add(count as usize);
                    exec_idx += 1;
                }
                Instruction::MoveLeft => {
                    address = address.wrapping_sub(1);
                    exec_idx += 1;
                }
                Instruction::MoveRight => {
                    address = address.wrapping_add(1);
                    exec_idx += 1;
                }
                Instruction::MoveToZero(direction,count) => {
                    if direction == Direction::Right {
                        while self.memory.get(address) != 0 {
                            address = address.wrapping_add(count as usize);
                        }
                    } else { 
                        while self.memory.get(address) != 0 {
                            address = address.wrapping_sub(count as usize);
                        }
                    }
                    exec_idx += 1;
                }
                Instruction::Read(count) => {
                    for i in 0..count {
                        self.memory.set(address, self.reader.read_or(0));
                    }
                    exec_idx += 1;
                }
                Instruction::Write(count) => {
                    let v = self.memory.get(address);
                    for i in 0..count {
                        self.writer.write(v);
                    }
                    exec_idx += 1;
                }
                Instruction::ReadBuf(count) => {
                    self.memory.set(address, self.reader.read_or(0));
                    for i in 0..count {
                        address = address.wrapping_add(1);
                        self.memory.set(address, self.reader.read_or(0));
                    }
                    exec_idx += 1;
                }
                Instruction::WriteBuf(count) => {
                    self.writer.write(self.memory.get(address));
                    for i in 0..count {
                        address = address.wrapping_add(1);
                        self.writer.write(self.memory.get(address));
                    }
                    exec_idx += 1;
                }
                Instruction::ReadLine => {
                    self.memory.set(address, 0);
                    address = address.wrapping_add(1);
                    let mut current = self.reader.read_or(0);
                    self.memory.set(address, current);
                    if current != 0 {
                        let start = address;
                        while current != 0 {
                            address = address.wrapping_add(1);
                            current = self.reader.read_or(0);
                            self.memory.set(address, current);
                        }
                        address = start;
                    }
                    exec_idx += 1;
                }
                Instruction::Noop(count, pattern) => {
                    // Nothing needs to be done here.
                    exec_idx += 1;
                }
            }
        }
        Ok(())
    }

    fn reset(&mut self) {
        self.reader.reset();
        self.writer.reset();
        self.memory.clear();
    }
}

impl BasicInterpreter {
    pub fn new() -> Self {
        Interpreter::with(
            BufferedReader::new(),
            Utf8Writer::new(),
            VirtualMemory::new()
        )
    }
}

impl<RT: BrainfuckReader, WT: BrainfuckWriter, MT: BrainfuckMemory> Interpreter<RT,WT,MT> {
    pub fn with(reader: RT, writer: WT, memory: MT) -> Self {
        Interpreter {
            reader,
            writer,
            memory,
        }
    }
}

#[test]
pub fn quicktest() {
    let mut interp = Interpreter::new();
}