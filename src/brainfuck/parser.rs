use std::{num::NonZeroUsize};

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
}

// [-]>,[>,]<[<]>
#[inline(always)]
fn check_readline(code: &[OpCode]) -> Option<NonZeroUsize> {
    if code.starts_with(seq::READLINE_SEQ) {
        return NonZeroUsize::new(seq::READLINE_SEQ.len());
    }
    None
}
// [-] or [+]
#[inline(always)]
fn check_reset_and_assign(code: &[OpCode]) -> Option<u8> {
    if code.len() >= 3
    && code[0] == op![jump]
    && code[2] == op![return]
    && op![add,sub].contains(&code[1]) {
        // Check whether or not there is an add/sub instruction immediately
        // after. If there is, that means that this will be an assignment
        if code.len() >= 4 {
            let mut sum = 0_i32;
            for &inst in code[3..].iter() {
                match inst {
                    op![add] => sum += 1,
                    op![sub] => sum -= 1,
                    _ => break,
                }
            }
            if sum != 0 {
                return Some(sum.rem_euclid(256) as u8);
            }
        }
        return Some(0);
    } else {
        None
    }
}
// [,]
#[inline(always)]
fn check_endread(code: &[OpCode]) -> bool {
    code.starts_with(seq::END_READ_SEQ)
}

// [<] | [>] | [<<<] | [>>>] etc.
fn check_movetozero(code: &[OpCode]) -> Option<(Direction, u32)> {
    // Because this code is called from inside Program::Load, it can assume
    // that the code is correct. It can also assume that the first instruction
    // is Jump.
    let lr = op![left,right];
    let dir = code[1];
    if !lr.contains(&dir) {
        return None;
    }
    let mut count = 0;
    for &inst in code[1..].iter() {
        if inst == dir {
            count += 1;
        } else if inst == op![return] {
            let dir = match dir {
                op![left] => Direction::Left,
                op![right] => Direction::Right,
                _ => panic!("This should be unreachable."),
            };
            return Some((dir, count));
        } else {
            return None;
        }
    }
    None
}

pub fn load(source: &str) -> Result<Box<[Instruction]>, SyntaxError> {
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

                let more = code.len() - inst_idx;
                if more >= 3
                && code[inst_idx+1] == op![right]
                && code[inst_idx+2] == action {
                    inst_idx += 3;
                    let mut count = 1;
                    //?>?
                    while (inst_idx + 2) <= code.len()
                    && code[inst_idx] == op![right]
                    && code[inst_idx+1] == action {
                        count += 1;
                        inst_idx += 2;
                    }
                    program.push(match action {
                        op![read] => Instruction::ReadBuf(count),
                        op![write] => Instruction::WriteBuf(count),
                        _ => unreachable!(),
                    });
                } else {
                    let start = inst_idx;
                    inst_idx += 1;
                    while inst_idx < code.len() && code[inst_idx] == action {
                        inst_idx += 1;
                    }
                    let count = inst_idx - start;
                    program.push(match action {
                        op![read] => Instruction::Read(count as u32),
                        op![write] => Instruction::Write(count as u32),
                        _ => unreachable!(),
                    });
                }
            }
            // [
            op![jump] => {
                if check_endread(&code[inst_idx..]) {
                    program.push(Instruction::EndRead);
                    inst_idx += 3;
                } else if let Some(n) = check_reset_and_assign(&code[inst_idx..]) {
                    inst_idx += 3;
                    if n != 0 {
                        inst_idx += n as usize;
                        program.push(Instruction::Assign(n));
                    } else {
                        inst_idx += 3;
                        program.push(Instruction::Reset);
                    }
                } else if let Some((dir, count)) = check_movetozero(&code[inst_idx..]) {
                    program.push(Instruction::MoveToZero(dir, count));
                    inst_idx += (count + 2) as usize;
                } else if let Some(length) = check_readline(&code[inst_idx..]) {
                    program.push(Instruction::ReadLine);
                    inst_idx += length.get();
                } else {
                    jump_stack.push(program.len());
                    // Temporary variable. This will be modified later.
                    program.push(Instruction::JumpZ(0));
                    inst_idx += 1;
                }
            }
            // ]
            op![return] => {
                // The index of the matching JumpZ instruction exists on the top of
                // `jump_stack`.
                if let Some(open_idx) = jump_stack.pop() {
                    let close_idx = program.len();
                    if let Instruction::JumpZ(jump_idx) = &mut program[open_idx] {
                        *jump_idx = (close_idx as u32) + 1;
                        program.push(Instruction::ReturnIf((open_idx as u32) + 1));
                        inst_idx += 1;
                    } else {
                        unreachable!("This shouldn't have happened.");
                    }
                } else {
                    // This should be impossible because the code should have already been verified.
                    panic!("Mismatched bracket.");
                }
            }
            _ => (),
        }
    }
    Ok(program.into_boxed_slice())
}


/// If there was no error, it will return the program op codes.
fn lex(source: &str) -> Result<Vec<OpCode>, SyntaxError> {
    if source.is_empty() {
        return Err(SyntaxError::NoInstructions);
    }
    let mut instructions: Vec<OpCode> = Vec::new();
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
/// Brainfuck instruction, and all brackets must be
/// in matching pairs.
pub fn verify(source: &str) -> bool {
    let mut stack_index = 0;

    let mut has_instruction = false;

    for c in source.chars() {
        match c {
            '>' | '<' | '+' | '-' | '.' | ',' => has_instruction = true,
            // [
            '[' => stack_index += 1,
            // ]
            ']' => {
                if stack_index > 0 {
                    has_instruction = true;
                    stack_index -= 1;
                } else {
                    return false;
                }
            }
            _ => (),
        }
    }
    // Check if stack_index is zero, which indicates that all
    // brackets were matched.
    has_instruction && stack_index == 0
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Direction {
    Left,
    Right,
}

// TODO:
//  Set to value: [-]+++
//  
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    // [-]++++
    Assign(u8),
    // IO
    // Repititions of ',' instructions.
    Read(u32),
    // Repitions of '.' instruction
    Write(u32),
    // There are also IO cases where you may read many bytes
    // ex: ,>,>,>, => Readbuf(4)
    // or: ,>,>,>,> => [ReadBuf(4),MoveRight]
    // This should only count up to the last read instruction.
    // Note: The cell pointer also moves with the count param.
    ReadBuf(u32),
    // .>.>.>.>.
    WriteBuf(u32),
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

    /// [-]>,[>,]<[<]>
    pub const READLINE_SEQ: &[OpCode] = &op![loop,-,return,>,in,loop,>,in,return,<,loop,<,return,>];

    pub const RESET_SEQ: (&[OpCode], &[OpCode]) = (
        &op![jump,-,return],
        &op![jump,+,return]
    );

    pub const END_READ_SEQ: &[OpCode] = &op![jump,in,return];

    pub const BUFFERED_READ_SEQ: &[OpCode] = &op![right, read];
    pub const BUFFERED_WRITE_SEQ: &[OpCode] = &op![right, write];

}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxError {
    // DisappearingStack,
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

impl  std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Instruction::Add(count) => write!(f, "Instruction::Add({count})"),
            Instruction::EndRead => write!(f, "Instruction::EndRead"),
            Instruction::JumpZ(idx) => write!(f, "Instruction::JumpZ({idx})"),
            Instruction::Move(count) => write!(f, "Instruction::Move({count})"),
            Instruction::MoveLeft => write!(f, "Instruction::MoveLeft"),
            Instruction::MoveRight => write!(f, "Instruction::MoveRight"),
            Instruction::MoveToZero(dir, count)=> write!(f, "Instruction::MoveToZero({},{count})", match dir { &Direction::Left => "Left", &Direction::Right => "Right" }),
            Instruction::Noop(count, pat) => write!(f, "Instruction::Noop({count})"),
            Instruction::Read(count)  => write!(f, "Instruction::Read({count})"),
            Instruction::ReadBuf(count) => write!(f, "Instruction::ReadBuf({count})"),
            Instruction::ReadLine => write!(f, "Instruction::ReadLine"),
            Instruction::Reset => write!(f, "Instruction::Reset"),
            Instruction::ReturnIf(addr) => write!(f, "Instruction::ReturnIf({addr})"),
            Instruction::Write(count) => write!(f, "Instruction::Write({count})"),
            Instruction::WriteBuf(count) => write!(f, "Instruction::WriteBuf({count})"),
            Instruction::Assign(value) => write!(f, "Instruction::Assign({value})"),
            _ => todo!(),
        }
    }
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
            // SyntaxError::DisappearingStack => writeln!(
            //     f,
            //     "You wouldn't believe this... the stack just disappeared!"
            // ),
        }
    }
}