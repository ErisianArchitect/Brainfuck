pub fn read_line() -> String {
    let mut input = String::new();
    if let Err(error) = std::io::stdin().read_line(&mut input) {
        println!("Error: {}", error);
        return String::new();
    }
    if let Some(mut nl_index) = input.find("\n") {
        // For Windows line endings (\r\n)
        if &input[nl_index-1..nl_index] == "\r" { 
            nl_index -= 1;
        }
        input[..nl_index].into()
    } else {
        input
    }
}

/// Reads a line from stdin, then parses special escape sequences from that line.
/// 
/// **Supported Sequences**:
/// 
/// ```text
/// \n      => newline
/// \t      => tab
/// \r      => Carriage Return
/// \0      => Null
/// \\      => Backslash
/// \xff    => Hex Byte
/// ```
pub fn bf_read_line() -> String {
    let mut line = read_line();
    let chars = Vec::<char>::from_iter(line.chars());
    let mut result = Vec::<char>::new();
    let mut index: usize = 0;

    // We want to be able to read special values:
    // \n
    // \r
    // \t
    // \0
    // \\
    // \xff
    // If the syntax is invalid, we'll just read it as plain text.

    while index < chars.len() {
        let c = chars[index];
        // This means that it may be a valid escape sequence
        if c == '\\' && (index + 1) < chars.len() {
            match chars[index + 1] {
                cc @ ('n' | 'r' | 't' | '0' | '\\') => {
                    result.push(match cc {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        '\\' => '\\',
                        // This branch will never happen, but Rust complains if it doesn't exist.
                        _ => unreachable!("How the fuck did you make this happen?"),
                    });
                    index += 2;
                },
                'x' if (index + 3) < chars.len() => {
                    if let Some(hex) = bf_chars_hex(&chars[index+2..index+4]) {
                        result.push(hex as char);
                        index +=  4;
                    } else {
                        result.extend_from_slice(&chars[index..index+4]);
                        index += 4;
                    }
                },
                other => {
                    result.push('\\');
                    result.push(other);
                    index += 2;
                },
            }
        } else {
            result.push(c);
            index += 1;
        }
    }

    String::from_iter(result.into_iter())
}

pub trait BrainfuckReader {
    fn read(&mut self) -> Option<u8>;

    fn read_or(&mut self, default: u8) -> u8 {
        self.read().unwrap_or(default)
    }

    fn reset(&mut self) {}
}

pub trait BrainfuckWriter {
    fn write(&mut self, value: u8);
    fn reset(&mut self) {}
}

pub struct BufferedReader {
    buffer: Option<Vec<u8>>,
    index: usize,
    readfunc: fn() -> String,
}

impl BrainfuckReader for BufferedReader {

    fn reset(&mut self) {
        self.buffer = None;
        self.index = 0;
    }

    fn read(&mut self) -> Option<u8> {
        if let Some(ref buffer) = self.buffer {
            if self.index < buffer.len() {
                let res = Some(buffer[self.index]);
                self.index += 1;
                return res;
            } else {
                self.buffer = None;
                return None;
            }
        } else {
            let readline = self.readfunc;
            self.buffer = Some(Vec::from_iter(readline().bytes()));
            self.index = 0;
            return self.read();
        }
    }
}

impl BufferedReader {

    pub fn with(readline: fn() -> String) -> BufferedReader {
        BufferedReader {
            buffer: None,
            index: 0,
            readfunc: readline,
        }
    }

    pub fn new() -> BufferedReader {
        BufferedReader::with(bf_read_line)
    }

    pub fn peek(&self) -> Option<u8> {
        if let Some(ref buffer) = self.buffer {
            if self.index < buffer.len() {
                return Some(buffer[self.index]);
            }
        }
        None
    }

}

pub fn bf_write_char(chr: char) {
    use std::{io::Write};
    print!("{}", chr);
    std::io::stdout().flush();
}

/// This class allows for unicode characters to be written to stdout.
/// Characters are written one byte at a time.
/// The characters are decoded from utf-8 sequences.
pub struct Utf8Writer {
    /// The function used to write characters.
    writeout: fn(char) -> (),
    /// The number of bytes buffered for the next character.
    buffered: u8,
    /// The number of bytes to expect for the current character.
    expect: u8,
    /// This is the value that is modified as new code points are added.
    codepoint: u32,
}

impl BrainfuckWriter for Utf8Writer {

    fn reset(&mut self) {
        self.buffered = 0;
        self.expect = 0;
        self.codepoint = 0;
    }
    
    fn write(&mut self, chr: u8) {
        // https://en.wikipedia.org/wiki/UTF-8#Encoding
        // If expect is 0, that means that we aren't in the middle of reading a character.
        let writeout = self.writeout;
        if (chr & 0b11000000) != 0b10000000 {
            // This means the currently buffering character is invalid
            // Write the error character out and set expect to 0.
            // We then continue as if nothing happened.
            if self.expect != 0 {
                self.expect = 0;
                writeout(Utf8Writer::ERROR_CHAR);
            }
            if (chr & 0b11000000) == 0b11000000 {
                self.buffered = 1;
                if (chr & 0b11100000) == 0b11000000 {
                    self.expect = 2;
                    self.codepoint = (chr & 0b11111) as u32;
                } else if (chr & 0b11110000) == 0b11100000 {
                    self.expect = 3;
                    self.codepoint = (chr & 0b1111) as u32;
                } else if (chr & 0b11111000) == 0b11110000 {
                    self.expect = 4;
                    self.codepoint = (chr & 0b111) as u32;
                } else {
                    // There was an error, so we'll just write out the error character.
                    writeout(Utf8Writer::ERROR_CHAR);
                }
            } else { // 7-bit ascii
                writeout(chr as char);
            }
        } else {
            if self.expect != 0 {
                self.codepoint = (self.codepoint << 6) | ((chr & 0b111111) as u32);
                self.buffered += 1;
                if self.buffered == self.expect {
                    if let Some(result) = char::from_u32(self.codepoint) {
                        writeout(result);
                    } else {
                        writeout(Utf8Writer::ERROR_CHAR)
                    }
                    self.expect = 0;
                    self.codepoint = 0;
                    self.buffered = 0;
                }
            } else {
                writeout(Utf8Writer::ERROR_CHAR);
            }
        }
    }
    
}

impl Utf8Writer {

    pub const ERROR_CHAR: char = '�';

    pub fn new() -> Utf8Writer {
        Utf8Writer::with(bf_write_char)
    }

    pub fn with(writefunc: fn(char) -> ()) -> Utf8Writer {
        Utf8Writer {
            writeout: writefunc,
            buffered: 0,
            expect: 0,
            codepoint: 0,
        }
    }

}

#[cfg(test)]
mod tests {
    use super::{Utf8Writer, BrainfuckWriter};


    #[test]
    fn test1() {
        use self::BrainfuckWriter;
        let mut writer = Utf8Writer::new();
        writer.write(225); // 0b11100001
        writer.write(154); // 0b10011010
        writer.write(179); // 0b10110011

        writer.write(225); // 0b11100001
        writer.write(154); // 0b10011010
        writer.write(185); // 0b10111001

        writer.write(225); // 0b11100001
        writer.write(154); // 0b10011010
        writer.write(171); // 0b10101011

        writer.write(255);
        writer.write(225); // 0b11100001
        writer.write(154); // 0b10011010
        writer.write(166); // 0b10100110

        writer.write(b'\n');
    }

}

/*
fn test() {
    let writer = Utf8Writer::new();
    // .>.>.>.
    writer.write(cell)
    env.incptr()
    writer.write(cell)
    env.incptr()
    writer.write(cell)
    env.incptr()
    writer.write(cell)
}
*/

fn bf_char_hex(chr: u8) -> u8 {
    match chr {
        b'0'..=b'9' => chr - b'0',
        b'a'..=b'f' => chr - b'a',
        b'A'..=b'F' => chr - b'A',
        _ => 0,
    }
}

fn bf_chars_hex(hex: &[char]) -> Option<u8> {
    if hex.len() == 2
    && hex[0].is_ascii_hexdigit()
    && hex[1].is_ascii_hexdigit() {
        Some((bf_char_hex(hex[0] as u8) << 4) | bf_char_hex(hex[1] as u8))
    } else {
        None
    }
}