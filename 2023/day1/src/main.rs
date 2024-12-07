use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn read_input<T, F>(path: &str, f: F) -> Result<Vec<T>, std::io::Error>
where
    F: Fn(String) -> T,
{
    let file = File::open(path)?;
    let reader = BufReader::new(file);

    let lines = reader.lines().filter_map(|line| line.ok()).map(f).collect();

    Ok(lines)
}

fn map1(s: String) -> u32 {
    let temp: String = s.chars().filter(|ch| ch.is_digit(10)).collect();

    format!(
        "{}{}",
        temp.chars().next().unwrap(),
        temp.chars().last().unwrap()
    )
    .parse()
    .unwrap()
}

fn map2(s: String) -> u32 {
    let mut m = s.clone();
    let numbers = vec![
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    loop {
        let t: Option<(usize, usize)> = numbers
            .clone()
            .into_iter()
            .enumerate()
            .filter_map(|(i, n)| m.find(n).map(|x| (x, i)))
            .min_by(|(x, _), (y, _)| x.cmp(y));

        match t {
            Some((_, i)) => m = m.replace(numbers[i], &format!("{}{}", i + 1, &numbers[i][1..])),
            None => break,
        }
    }
    map1(m)
}

fn main() -> std::io::Result<()> {
    let p1: u32 = read_input("input", map1).unwrap().into_iter().sum();
    let p2: u32 = read_input("input", map2).unwrap().into_iter().sum();

    println!("Part 1: {p1}");
    println!("Part 2: {p2}");
    Ok(())
}
