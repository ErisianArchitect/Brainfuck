use std::fmt::Display;

pub trait Printer {
    fn print(&self) {}
    fn write(&self) {}
}

impl<T: Display> Printer for T {
    fn print(&self) {
        println!("{}", self);
    }

    fn write(&self) {
        print!("{}", self);
    }
}

pub trait PrintWith<T> {
    fn print_with(&self, tail: T) {}

    fn pw(&self, tail: T) {
        self.print_with(tail);
    }
}

// impl<Head: Printer, Tail: Printer + Copy> PrintWith<Tail> for Head {
//     fn print_with(&self, tail: Tail) {
//         self.write();
//         tail.print();
//     }
// }

impl<Head: Printer, Tail: Printer> PrintWith<Tail> for Head {
    fn print_with(&self, tail: Tail) {
        self.write();
        tail.print();
    }
}

pub fn print<T: Printer>(value: &T) {
    value.print();
}