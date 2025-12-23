use std::{fs, io};

const INPUT: &'static str = "input";

pub fn read_input<F, T>(f: F) -> io::Result<Vec<T>>
where
    F: Fn(&str) -> T,
{
    let contents = fs::read_to_string(INPUT)?;
    let lines: Vec<&str> = contents.lines().collect();

    let mut out: Vec<T> = Vec::new();

    for line in lines {
        out.push(f(&line));
    }

    Ok(out)
}

pub fn read_lines() -> io::Result<Vec<String>> {
    return read_input(|line| line.to_owned());
}

pub const DIAGS: [(i32, i32); 4] = [(-1, -1), (-1, 1), (1, -1), (1, 1)];
pub const CARDINALS: [(i32, i32); 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];
pub const ADJ4: [(i32, i32); 4] = CARDINALS;
pub const ADJ8: [(i32, i32); 8] = [
    (-1, -1),
    (-1, 1),
    (1, -1),
    (1, 1),
    (0, -1),
    (0, 1),
    (-1, 0),
    (1, 0),
];

pub fn fixpoint<T, F>(mut x: T, f: F) -> T
where
    T: Eq,
    F: Fn(&T) -> T,
{
    loop {
        let next = f(&x);
        if next == x {
            return x;
        }
        x = next;
    }
}
