// Some code inspired by: https://github.com/Overv/bf
// #![allow(dead_code, unused)]
// This is the module for the Brainfuck interpreter.

use std::{collections::HashMap, num::NonZeroUsize};

/*
I would like to do a little experimentation with Brainfuck computation.
It's said that Brainfuck is turing complete, so that implies that any
program can be written in it given infinite time and resources and the
right backdoors.

But anyway, I can design a computation model that will allow more
complex programs to be written.

One particular idea I had for that computation model was that memory
can be segmented into blocks that the brainfuck program can understand.
I can be said that brainfuck has to feel its way around the memory.
So these blocks would consist of various segments:
compute registers | memory bus |memory

If I design my Brainfuck programs in that manner, I can write this
interpreter to be pretty fast compared to other Brainfuck interpreters.
*/

// The way I would like to design the memory is so that it
// seems to be infinite even though it is limited.
pub struct MemoryBlock {
    data: Option<Box<[u8; MemoryBlock::BLOCK_SIZE]>>,
    used: usize,
}

impl MemoryBlock {
    /// This represents the number of bytes `MemoryBlock` will hold.
    /// This value should be a power of two.
    pub const BLOCK_SIZE: usize = 1024;

    pub fn new() -> MemoryBlock {
        MemoryBlock {
            data: None,
            used: 0,
        }
    }

    /// Returns true if this `MemoryBlock` is allocated.
    /// This means that at least one element is non-zero.
    pub fn allocated(&self) -> bool {
        self.data.is_some()
    }

    /// Returns true if this `MemoryBlock` is unallocated.
    pub fn unallocated(&self) -> bool {
        self.data.is_none()
    }

    /// Returns the number of cells that are non-zero.
    pub fn used(&self) -> usize {
        self.used
    }

    /// Drops the memory used by `MemoryBlock`.
    pub fn clear(&mut self) {
        self.data = None;
        self.used = 0;
    }

    pub fn get(&self, offset: usize) -> u8 {
        // Allows us to wrap the offset to the block size.
        let offset = MemoryBlock::sub_index(offset);
        if let Some(data) = self.data.as_ref() {
            if let Some(value) = data.get(offset) {
                return *value;
            }
        }
        // By default, if the memory block is not present, return 0.
        0
    }

    pub fn set(&mut self, offset: usize, value: u8) {
        // Allows us to wrap the offset to the block size.
        let offset = MemoryBlock::sub_index(offset);
        if let Some(data) = self.data.as_deref_mut() {
            // If a cell is becoming non-zero
            if data[offset] == 0 && value != 0 {
                self.used += 1;
            }
            // If a cell is becoming zero
            else if data[offset] != 0 && value == 0 {
                self.used -= 1;
            }
            data[offset] = value;
        } else if value != 0 {
            self.data = Some(Box::new([u8::default(); MemoryBlock::BLOCK_SIZE]));
            // It's safe to call unwrap because we just assigned it.
            let mut data = self.data.as_mut().unwrap();
            data[offset] = value;
            self.used = 1;
        }
        if self.used == 0 && self.data.is_some() {
            self.data = None;
        }
    }

    /// Wraps `offset` to `BLOCK_SIZE`.
    fn sub_index(offset: usize) -> usize {
        offset.rem_euclid(MemoryBlock::BLOCK_SIZE)
    }
}

struct Memory {
    blocks: HashMap<usize, MemoryBlock>,
}

impl Memory {
    fn new() -> Memory {
        let mut blocks = HashMap::new();
        // insert memory blocks for
        blocks.insert(0_usize, MemoryBlock::new());
        Memory { blocks }
    }

    fn block_index(offset: usize) -> usize {
        offset.div_euclid(MemoryBlock::BLOCK_SIZE)
    }

    fn get(&self, offset: usize) -> u8 {
        let key = Memory::block_index(offset);
        if let Some(block) = self.blocks.get(&key) {
            return block.get(offset);
        }
        0
    }

    fn set(&mut self, offset: usize, value: u8) {
        let key = Memory::block_index(offset);
        if let Some(block) = self.blocks.get_mut(&key) {
            block.set(offset, value);
        } else if value != 0 {
            let mut block = MemoryBlock::new();
            block.set(offset, value);
            self.blocks.insert(key, block);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxError {
    DisappearingStack,
    NoInstructions,
    EndWithNoBeginning {
        index: u32,
        line_no: u32,
        col_no: u32,
    },
    BeginningWithNoEnd {
        index: u32,
        line_no: u32,
        col_no: u32,
    },
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            SyntaxError::NoInstructions => writeln!(
                f,
                "Syntax Error: There were no Brainfuck instructions in the source string."
            ),
            SyntaxError::BeginningWithNoEnd {
                index,
                line_no,
                col_no,
            } => writeln!(
                f,
                "Syntax Error[{}:{}]: Found an open bracket with no close bracket at index {}",
                line_no, col_no, index
            ),
            SyntaxError::EndWithNoBeginning {
                index,
                line_no,
                col_no,
            } => writeln!(
                f,
                "Syntax Error[{}:{}]: Found a close bracket with no open bracket at index {}",
                line_no, col_no, index
            ),
            SyntaxError::DisappearingStack => writeln!(
                f,
                "You wouldn't believe this... the stack just disappeared!"
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpCode {
    MoveRight, // >    Increment the cell pointer
    MoveLeft,  // <    Decrement the cell pointer
    Increment, // +    Increment u8 cell
    Decrement, // -    Decrement u8 cell
    Read,      // ,    Read byte from terminal
    Write,     // .    Write byte to terminal
    JumpZ,     // [    Jump to matching ret if cell is zero
    Return,    // ]    Return to matching JumpZ if cell is non-zero
}

macro_rules! op {
    [<] => {op![left]};
    [>] => {op![right]};
    [+] => {op![add]};
    [-] => {op![sub]};
    [,] => {op![in]};
    [.] => {op![out]};
    [:] => {op![jump]};
    [;] => {op![return]};
    [in] => {op![read]};
    [out] => {op![write]};
    [jmp] => {op![jump]};
    [ret] => {op![return]};
    [loop] => {op![jump]};
    [left] => {OpCode::MoveLeft};
    [right] => {OpCode::MoveRight};
    [add] => {OpCode::Increment};
    [sub] => {OpCode::Decrement};
    [read] => {OpCode::Read};
    [write] => {OpCode::Write};
    [jump] => {OpCode::JumpZ};
    [return] => {OpCode::Return};
    [$($a:tt)|+] => {
        $(op![$a])|+
    };
    [$($i:tt),+] => {
       [$(op![$i]),+]
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    // Moving pointer
    Move(i32), // cell_pointer += move.0
    MoveRight, // cell_pointer++
    MoveLeft,  // cell_pointer--
    // Special Move mechanisms
    // MoveToZero will keep moving the cell pointer until it
    // finds an empty cell.
    // Syntax:
    //  [<] | [>] | [>>>] | [<<<]
    MoveToZero(Direction, u32),
    // Cell Mutation
    Add(u8), // wrapped value of combined +/- instructions
    // IO
    Read(u32),
    Write(u32),
    // There are also IO cases where you may read many bytes
    // ex: ,>,>,>, => Readbuf(4)
    // or: ,>,>,>,> => [ReadBuf(4),MoveRight]
    // This should only count up to the last read instruction.
    // Note: The cell pointer also moves with the count param.
    ReadBuf(u32),
    // .>.>.>.>.
    Writebuf(u32),
    // ReadLine syntax:
    // [-]>,[>,]<[<]>
    // First set the current cell to zero, then move right.
    // Read and move right in a loop until zero is read (EOL)
    // Then return to the empty cell at the start
    // Move to the right to place the pointer at the start of
    // The line that was just read.
    ReadLine,
    // EndRead syntax:
    // [,]
    // This will cause all remaining characters on the line to be read.
    EndRead,
    // Jump ahead to index if cell is zero.
    JumpZ(u32),
    // Return to jump at index if cell is nonzero
    ReturnIf(u32),
    // Reset Syntax:
    // [-] or [+]
    // The reason either syntax will work is due to the wrapping
    // functionality.
    // The reset instruction is a shortcut for the above syntax.
    // What the above syntax does is it loops over the cell,
    // decrementing it with each iteration, until it is zero.
    Reset,
    // No-Operation
    // Syntax:
    // 1: >< | <>
    // 2: ><>< | <><> | >><< | <<>> | <>>< | ><<>
    // 3: ><><>< | <><><> | >>><<< | <<<>>>
    // 4: ...etc.
    // As long as the number of lefts equal the number of rights, it will work.
    // This also works with +/-.
    Noop(u16, NoopPattern),
    // Loop Forever:
    // [] | [noop*]
}

/// Having multiple Noop (no operation) patterns means that you can
/// extend the functionality of the interpreter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum NoopPattern {
    // ><
    Bowtie,
    // <>
    Gem,
    // <><>
    Infinity,
    // ><<>
    RightFish,
    // <>><
    LeftFish,
    // ><><><
    TwoFish,
    // <<<>>>
    Turbo,
    // <>><<>
    Eyes,
    // ><><><><><><><
    Ramona,
    // +--+
    Barbell,
    // -++-
    Bridge,
    // +--++--+
    LongBridge,
    // +-
    AddMinus,
    // -+
    MinusAdd,
    // Any pattern that has no effect on program state
    Any,
}

macro_rules! noop {
    [><] => { noop![bowtie] };
    [bowtie] => { NoopPattern::Bowtie };
    [<>] => { noop![gem] };
    [gem] => { NoopPattern::Gem };
    [<><>] => { noop![infinity] };
    [infinity] => { NoopPattern::Infinity };
    [><<>] => { noop![rfish] };
    [rfish] => { NoopPattern::RightFish };
    [<>><] => { noop![lfish] };
    [lfish] => { NoopPattern::LeftFish };
    [><><><] => { noop![two] };
    [two] => { NoopPattern::TwoFish };
    [<<<>>>] => { noop![turbo] };
    [turbo] => { NoopPattern::Turbo };
    [<>><<>] => { noop![eyes] };
    [eyes] => { NoopPattern::Eyes };
    [><><><><><><><] => { noop![ramona] };
    [ramona] => { NoopPattern::Ramona };
    [+--+] => { noop![barbell] };
    [barbell] => { NoopPattern::Barbell };
    [-++-] => { noop![bridge] };
    [bridge] => { NoopPattern::Bridge };
    [+--++--+] => { noop![longbridge] };
    [longbridge] => { NoopPattern::LongBridge };
    [+-] => { noop![add] };
    [add] => { NoopPattern::AddMinus };
    [-+] => { noop![sub] };
    [sub] => { NoopPattern::MinusAdd };
    // New designs come after this line.
    [<+--+>] => {};
    [<-++->] => {};
    [>-++-<] => {};
    [>+--+<] => {};
    [+-<>-+] => {};
    [-+><+-] => {};
    [<+-<+->>] => {};

}

mod seq {
    use super::OpCode;

    macro_rules! noop_seq {
        [bowtie] => {
            op![>,<]
        };
        [gem] => {
            op![<,>]
        };
        [infinity] => {
            op![<,>,<,>]
        };
        [rfish] => {
            op![>,<,<,>]
        };
        [lfish] => {
            op![<,>,>,<]
        };
        [two] => {
            op![>,<,>,<,>,<]
        };
        [turbo] => {
            op![<,<,<,>,>,>]
        };
        [eyes] => {
            op![<,>,>,<,<,>]
        };
        [ramona] => {
            op![>,<,>,<,>,<,>,<,>,<,>,<,>,<]
        };
        [barbell] => {
            op![+,-,-,+]
        };
        [bridge] => {
            op![-,+,+,-]
        };
        [longbridge] => {
            op![+,-,-,+,+,-,-,+]
        };
        [add] => {
            op![+,-]
        };
        [sub] => {
            op![-,+]
        };
    }

    macro_rules! const_noop_seq {
        [$i:ident: $s:ident] => { pub const $i: &[OpCode] = &noop_seq![$s]; };
    }

    const_noop_seq![BOWTIE_SEQ: bowtie];
    const_noop_seq![GEM_SEQ: gem];
    const_noop_seq![_SEQ: infinity];
    const_noop_seq![RFISH_SEQ: rfish];
    const_noop_seq![LFISH_SEQ: lfish];
    const_noop_seq![TWO_SEQ: two];
    const_noop_seq![TURBO_SEQ: turbo];
    const_noop_seq![EYES_SEQ: eyes];
    const_noop_seq![RAMONA_SEQ: ramona];
    const_noop_seq![BARBELL_SEQ: barbell];
    const_noop_seq![BRIDGE_SEQ: bridge];
    const_noop_seq![LONGBRIDGE_SEQ: longbridge];
    const_noop_seq![ADD_SEQ: add];
    const_noop_seq![SUB_SEQ: sub];

    pub const READLINE_SEQ: &[OpCode] = &op![loop,-,return,>,in,loop,>,in,return,<,loop,<,return,>];

    pub const RESET_SEQ: (&[OpCode], &[OpCode]) = (
        &op![jump,-,return],
        &op![jump,+,return]
    );

    pub const END_READ_SEQ: &[OpCode] = &op![jump,in,return];

}

// A program consists
pub struct Program {
    instructions: Box<[Instruction]>,
}

// [-]>[,>][<]>
#[inline(always)]
fn check_readline(code: &[OpCode]) -> Option<NonZeroUsize> {
    if code.starts_with(seq::READLINE_SEQ) {
        return NonZeroUsize::new(seq::READLINE_SEQ.len());
    }
    None
}
// [-] or [+]
#[inline(always)]
fn check_reset(code: &[OpCode]) -> Option<NonZeroUsize> {
    if code.starts_with(seq::RESET_SEQ.0) {
        return NonZeroUsize::new(seq::RESET_SEQ.0.len());
    } else if code.starts_with(seq::RESET_SEQ.1) {
        return NonZeroUsize::new(seq::RESET_SEQ.1.len());
    }
    None
}
// [,]
#[inline(always)]
fn check_endread(code: &[OpCode]) -> Option<NonZeroUsize> {
    if code.starts_with(seq::END_READ_SEQ) {
        return NonZeroUsize::new(seq::END_READ_SEQ.len());
    }
    None
}
// [<] | [>] | [<<<] | [>>>] etc.
fn check_movetozero(code: &[OpCode]) -> Option<(usize, Instruction)> {
    // If I wrote my code correctly, this test should always pass.
    if code[0] == op![jump] {
        let direction = match code[1] {
            op![<] => Direction::Left,
            op![>] => Direction::Right,
            // Early return
            _ => return None,
        };
        let mut count : usize = 1;
        for &inst in code[2..].iter() {
            // Unlikely to happen.
            match (direction, inst) {
                (Direction::Left, op![<]) => count += 1,
                (Direction::Right, op![>]) => count += 1,
                (_, op![return]) => break,
                _ => return None,
            }
        }
        // Just in case someone decides they want to cripple this program.
        if count > u32::MAX as usize  {
            println!("Sorry, you're not allowed to use {} move operations. That's just too much.", count);
            return None;
        }
        Some((count + 2, Instruction::MoveToZero(direction, count as u32)))
    }
    else {
        None
    }
}

impl Program {
    pub fn load(source: &str) -> Result<Program, SyntaxError> {
        // TODO: Add parsing for Noop instructions.
        use seq::*;
        let code = lex(source)?;
        // For when we enter [loops]
        let mut program = Vec::<Instruction>::new();
        let mut jump_stack = Vec::<usize>::new();
        //let Some(current) = loop_stack.first_mut();
        let mut inst_idx = 0;
        // I've added the loop variable in case I need to break.
        'outer: while inst_idx < code.len() {
            // Each arm handles moving the instruction index.
            match code[inst_idx] {
                op![+|-] => {
                    // Count all +/- opcodes
                    let mut sum = 0i32;
                    let mut adds = 0;
                    while inst_idx < code.len() {
                        match code[inst_idx] {
                            op![+] => {
                                adds += 1;
                                sum += 1;
                            }
                            op![-] => sum -= 1,
                            _ => break,
                        }
                        inst_idx += 1;
                    }
                    // If the sum is zero, that means that the overall result of all the increments
                    // and decrements is a difference of zero. (+-, -=, ++--, etc.)
                    if sum == 0 {
                        program.push(Instruction::Noop(adds, noop![add]))
                    } else {
                        program.push(Instruction::Add(sum.rem_euclid(256) as u8));
                    }
                }
                op![<|>] => {
                    let mut sum = 0i32;
                    let mut rights = 0;
                    while inst_idx < code.len() {
                        match code[inst_idx] {
                            op![>] => {
                                rights += 1;
                                sum += 1;
                            }
                            op![<] => {
                                sum -= 1;
                            }
                            _ => break,
                        }
                        inst_idx += 1;
                    }
                    match sum {
                        1 => program.push(Instruction::MoveRight),
                        -1 => program.push(Instruction::MoveLeft),
                        0 => program.push(Instruction::Noop(rights, noop![><])),
                        _ => program.push(Instruction::Move(sum)),
                    }
                }
                // , or . (read or write)
                action @ op![read|write] => {
                    match action {
                        // ,
                        op![read] => {

                        },
                        // .
                        op![write] =>{
                            
                        },
                        _ => (),
                    }
                }
                // [
                op![jump] => {
                    // There are multiple things that could happen at this point.
                    // Since there are multiple ways the following tokens can be
                    // interpreted, we will try our best to test for those cases before hand.
                    inst_idx += if let Some(length) = check_endread(&code[inst_idx..]) {
                        program.push(Instruction::EndRead);
                        length.get()
                    } else if let Some(length) = check_reset(&code[inst_idx..]) {
                        program.push(Instruction::Reset);
                        length.get()
                    } else if let Some((length, result)) = check_movetozero(&code[inst_idx..]) {
                        program.push(result);
                        length as usize
                    } else if let Some(length) = check_readline(&code[inst_idx..]) {
                        program.push(Instruction::ReadLine);
                        length.get()
                    }
                    // This means that no special syntax was found.
                    // If this is the case, we want to push the index of the next instruction
                    // into the `jump_stack`, which is a vector that is used to help in matching
                    // of square brackets.
                    // When the matching close bracket is found, the index of the open bracket is
                    // popped from `jump_stack`, then the original `JumpZ` instruction is modified
                    // to include the index of the close bracket. The instruction for the close
                    // bracket is also pushed into the program, and stores the index of the
                    // JumpZ instruction.
                    // which is `program.len()`, then push the JumpZ Instruction into `program`.
                    // The JumpZ instruction that is pushed to program will later be modified
                    // to include the matching end brace once that matching end brace is found.
                    else {
                        // push the length of the program which represents the index
                        // of the elements that is pushed on the next line.
                        jump_stack.push(program.len());
                        // Temporary variable. This will be modified later.
                        program.push(Instruction::JumpZ(0));
                        1
                    };
                }
                // ]
                op![return] => {
                    // The index of the matching JumpZ instruction exists on the top of
                    // `jump_stack`.
                    if let Some(open_idx) = jump_stack.pop() {
                        let close_idx = program.len();
                        match program[open_idx] {
                            Instruction::JumpZ(_) => {
                                program[open_idx] = Instruction::JumpZ(close_idx as u32);
                                program.push(Instruction::ReturnIf(open_idx as u32));
                                inst_idx += 1;
                            },
                            _ => panic!("I'm not sure how this happened, but the index popped off the top of jump_stack was an invalid instruction."),
                        }
                    }
                    else {
                        // This should be impossible because the code should have already been verified.
                        panic!("Mismatched bracket.");
                    }
                }
                _ => (),
            }
        }
        Ok(Program {
            instructions: program.into_boxed_slice(),
        })
    }
}

/// If there was no error, it will return the program op codes.
fn lex(source: &str) -> Result<Vec<OpCode>, SyntaxError> {
    if source.is_empty() {
        return Err(SyntaxError::NoInstructions);
    }
    let mut instructions = Vec::new();
    let mut stack_index = 0;
    let mut open_index = 0;
    let mut open_line_no = 0;
    let mut open_col_no = 0;
    let mut valid = true;
    // We will determine line number and column number with line_no and col_no respectively.
    let mut line_no = 1; // {line_no}:{col_no}
    let mut col_no = 1;
    for (i, c) in source.chars().enumerate() {
        match c {
            '>' => instructions.push(op![>]),
            '<' => instructions.push(op![<]),
            '+' => instructions.push(op![+]),
            '-' => instructions.push(op![-]),
            '.' => instructions.push(op![.]),
            ',' => instructions.push(op![,]),
            '[' => {
                stack_index += 1;
                open_index = i;
                open_line_no = line_no;
                open_col_no = col_no;
                instructions.push(op![jump]);
            }
            ']' => {
                if stack_index == 0 {
                    return Err(SyntaxError::EndWithNoBeginning {
                        index: i as u32,
                        line_no,
                        col_no,
                    });
                }
                stack_index -= 1;
                instructions.push(op![return]);
            }
            '\n' => {
                line_no += 1;
                col_no = 1;
                continue;
            }
            _ => (),
        }
        col_no += 1;
    }
    if instructions.is_empty() {
        return Err(SyntaxError::NoInstructions);
    }
    if stack_index != 0 {
        return Err(SyntaxError::BeginningWithNoEnd {
            index: open_index as u32,
            line_no: open_line_no,
            col_no: open_col_no,
        });
    }
    Ok(instructions)
}

/// Verifies that the provided source string can be interpreted
/// as a valid Brainfuck program. It must have at least one
/// Brainfuck instruction, and it must all brackets must be
/// in matching pairs.
fn verify(source: &str) -> bool {
    let mut stack_index = 0i32;

    let mut has_instruction = false;

    for c in source.chars() {
        match c {
            '>' | '<' | '+' | '-' | '.' | ',' => has_instruction = true,
            // [
            '[' => stack_index += 1,
            // ]
            ']' => {
                // This means that it is mismatched.
                match stack_index {
                    0 => return false,
                    1.. => has_instruction = true,
                    _ => (),
                }
                stack_index -= 1;
            }
            _ => (),
        }
    }
    // Check if stack_index is zero, which indicates that all
    // brackets were matched.
    has_instruction && stack_index == 0
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn lex_test() {
        let result = lex("+++++-----[][][+++][][[]][[[]]][[][]][[][[]][]]., THUNDERDOME !@$!@%!^");
        assert!(result.is_ok());
    }

    enum Quick {
        One(u32),
        Two(u32),
        Three(u32),
    }

    #[test]
    fn quick_test() {
        //use tests::Quick;
        let mut a = 1;
        let mut b = 2;
        let mut c = 3;
        let matcher = |n| {
            match n {
                1 => println!("One"),
                2 => println!("Two"),
                3.. => println!("Three or more."),
                _ => unreachable!("This won't be possible."),
            }
        };
        matcher(a);
        matcher(b);
        matcher(c);
    }

    fn plus_one(value: i32) -> i32 {
        value.wrapping_add(1)
    }

    #[test]
    fn misc_test() {
        println!("########################################");
        println!("Size: {}", std::mem::size_of::<Instruction>());
        println!("########################################");
        let a = Some(Some("body"));
        if let Some(Some(body)) = a {
            println!("Some {body} once told me.");
        }
    }


    macro_rules! fnord {
        ($($t:tt)*) => {
            $(print!("{}, ", stringify!($t)));*
        };
    }
    #[test]
    fn macro_test(){
        use seq::*;
        let inst = vec![op![jmp], op![,], op![ret]];
        //println!("{}", matches!(&inst[..], op![jmp, '>']));
    }

    #[test]
    fn memory_block() {
        // Create new MemoryBlock and make sure it is
        // not allocated and also that the used count is
        // zero.
        let mut mem = MemoryBlock::new();
        assert_eq!(mem.used(), 0);
        assert!(mem.unallocated());

        // The next test is to see if setting a cell to non-zero
        // will trigger allocation.

        mem.set(0, 1);
        assert_eq!(mem.used(), 1);
        // Test that the allocated function is working correctly.
        assert!(mem.allocated());

        // Next set the same cell back to zero to see if it will
        // trigger deallocation.

        mem.set(0, 0);
        assert_eq!(mem.used(), 0);
        assert!(mem.unallocated());

        // Now

        // Now we will set a single element to every non-zero
        // value and make sure that the used count remains
        // the same.
        for i in 1..=255 {
            mem.set(0, i);
            assert_eq!(mem.used(), 1);
            assert!(mem.allocated());
        }

        mem.set(0, 0);
        assert_eq!(mem.used(), 0);
        assert!(mem.unallocated());

        // Now we will set every element to 1 and see if the
        // used count increases with every assignment.
        for i in 0..MemoryBlock::BLOCK_SIZE {
            mem.set(i, 1);
            assert_eq!(mem.used(), i + 1);
            assert!(mem.allocated());
        }

        // Now test that the clear() function works.
        mem.clear();
        assert_eq!(mem.used(), 0);
        assert!(mem.unallocated());
    }
}
