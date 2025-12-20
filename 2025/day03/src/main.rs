fn max_joltage(bank: Vec<u8>, n: usize) -> u64 {
    let v = &bank[n..];

    let (i, &max) = v
        .iter()
        .enumerate()
        .max_by_key(|&(_, value)| value)
        .unwrap();

    let mut res = u64::pow(10, n as u32) * max as u64;

    if n > 0 {
        res += max_joltage(bank[0..n + i].to_vec(), n - 1);
    }

    return res;
}

fn main() {
    let input: Vec<Vec<u8>> = base::read_input(|line| {
        line.chars()
            .rev()
            .map(|c| c.to_digit(10).unwrap() as u8)
            .collect()
    })
    .unwrap();

    let mut sum1 = 0;
    let mut sum2 = 0;
    for l in input {
        sum1 += max_joltage(l.clone(), 1);
        sum2 += max_joltage(l.clone(), 11);
    }

    println!("{}", sum1);
    println!("{}", sum2);
}
