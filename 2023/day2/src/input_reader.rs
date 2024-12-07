use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

pub fn read_input<T, F>(path: &str, f: F) -> Result<Vec<T>, std::io::Error>
where
    F: Fn(String) -> T,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let lines = reader.lines().filter_map(|line| line.ok()).map(f).collect();

    Ok(lines)
}
