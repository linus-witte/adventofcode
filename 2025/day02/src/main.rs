use std::ops::RangeInclusive;

fn divisors(n: usize) -> Vec<usize> {
    let mut res = Vec::new();

    for i in 1..n {
        if n % i == 0 {
            res.push(i);
        }
    }
    return res;
}

fn part_a(ranges: Vec<RangeInclusive<u64>>) -> u64 {
    let mut res = 0;
    for range in ranges {
        for id in range {
            let s = id.to_string();
            let (left, right) = s.split_at(s.len() / 2);

            if left == right {
                res += id;
            }
        }
    }

    return res;
}

fn part_b(ranges: Vec<RangeInclusive<u64>>) -> u64 {
    let mut res = 0;
    for range in ranges {
        for id in range {
            let s = id.to_string();
            let divs = divisors(s.len());

            for d in divs {
                let n_slices = s.len() / d;
                let slices: Vec<&str> = (0..n_slices).map(|i| &s[i * d..(i + 1) * d]).collect();
                let all_equal = slices.windows(2).all(|w| w[0] == w[1]);

                if all_equal {
                    res += id;
                    break;
                }
            }
        }
    }

    return res;
}

fn main() -> std::io::Result<()> {
    let ranges: Vec<RangeInclusive<u64>> = base::read_lines()?
        .iter()
        .flat_map(|line| {
            line.split(',').map(|s| {
                let (a, b) = s.split_once('-').unwrap();
                a.parse::<u64>().unwrap()..=b.parse::<u64>().unwrap()
            })
        })
        .collect();

    println!("{}", part_a(ranges.clone()));
    println!("{}", part_b(ranges));

    Ok(())
}
