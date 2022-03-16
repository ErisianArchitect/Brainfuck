use crate::brainfuck::io::Utf8Writer;

pub struct Interpreter {
    reader: fn() -> u8,
    writefunc: fn(u8) -> (),
}