use std::fs;

fn main() {
    let file_path = "input";

    let contents = fs::read_to_string(file_path).unwrap();
    let input: Vec<&str> = contents.lines().collect();

    let numbers: Vec<i32> = input
        .iter()
        .filter_map(
            |&line| match (line.chars().next(), line[1..].parse::<i32>().ok()) {
                (Some('R'), Some(n)) => Some(n),
                (Some('L'), Some(n)) => Some(-n),
                _ => None,
            },
        )
        .collect();

    // a)
    let mut dial = 50;
    let mut i = 0;

    for n in numbers.clone().into_iter() {
        dial += n;
        if dial % 100 == 0 {
            i += 1;
        }
    }

    println!("Password: {}", i);

    // b)
    let mut dial = 50;
    let mut i = 0;

    for n in numbers.clone().into_iter() {
        let direction = n / n.abs();
        for _ in 0..n.abs() {
            dial += direction;
            if dial % 100 == 0 {
                i += 1;
            }
        }
    }
    println!("Password: {}", i);
}
