#![allow(dead_code, unused)]

use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::fmt::Display;
use std::io::Write;
use std::mem::size_of;
use std::ops::Index;
use std::rc::Rc;

mod printer;
use printer::*;

mod brainfuck;
use brainfuck::io::{BufferedReader, bf_read_line};


macro_rules! size {
    ($t:ty) => {
        println!("Size of {} is {}", stringify!($t), size_of::<$t>());
    };
}

fn main() {
    loop {
        std::io::stdout().lock().write_all(b">>> ");
        std::io::stdout().flush();
        let s = bf_read_line();
        println!("{}", s);
    }

    //reader_test();
}

fn reader_test() {
    let mut reader = BufferedReader::new();
    let c = reader.peek();
    if c.is_none() {
        println!("c.is_none == true")
    }

    let mut func = || {
        if let Some(ch) = reader.read() {
            println!("The character was {}! ({ch})", ch as char);
            true
        } else {
            println!("<EOF>");
            false
        }
    };
    while func() {}
    println!("New Line");
    func();
}

macro_rules! i {
    ($i:ident: $e:expr) => {
        println!("{}: {}", stringify!($i), $e);
    };
    ($s:literal: $e:expr) => {
        println!("{}: {}", $s, $e);
    };
}

fn sandbox2() {
    let mut r: Rc<RefCell<brainfuck::mem::MemoryBlock>> =
        Rc::<RefCell<brainfuck::mem::MemoryBlock>>::new(RefCell::new(brainfuck::mem::MemoryBlock::new()));
    (*r).borrow_mut().set(0, 1);
    r.as_ref().borrow_mut().set(0, 1);
    i!(Used: r.as_ref().borrow().used());
}

fn sandbox1() {

    let mut mem = brainfuck::mem::MemoryBlock::new();
    mem.allocated().print();
    mem.set(17, 43);
    mem.set(18, 43);
    mem.set(19, 43);
    i!("Test Info": "Hello, world!");
    i!(Test: "
    // This can be whatever code you want.
    #include<iostream>
    int main() {
        std::cout << \"Hello, world!\" << std::endl;
    }");
    i!("Allocated 1": mem.allocated());
    i!(Used: mem.used());
    mem.set(17, 0);
    mem.set(18, 0);
    i!("Allocated 2": mem.allocated());
    i!(Used: mem.used());
    mem.set(19, 0);
    i!("Allocated 3": mem.allocated());
    i!(Used: mem.used());
}
