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
