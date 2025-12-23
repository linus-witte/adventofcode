use std::ops::RangeInclusive;

fn merge_ranges(ranges: &Vec<RangeInclusive<u64>>) -> Vec<RangeInclusive<u64>> {
    ranges
        .iter()
        .fold(Vec::new(), |mut acc: Vec<RangeInclusive<u64>>, x| {
            let overlap = acc.iter().enumerate().find(|&(_, r)| {
                (x.start() <= r.start() && r.start() <= x.end())
                    || (r.start() <= x.start() && x.start() <= r.end())
            });

            match overlap {
                Some((idx, range)) => {
                    let updated = *range.start().min(x.start())..=*range.end().max(x.end());
                    acc.remove(idx);
                    acc.push(updated);
                }
                None => acc.push(x.clone()),
            };

            acc
        })
}

fn main() {
    let input = base::read_lines().unwrap();
    let mut iter = input.split(|x| x.is_empty());

    let ranges: Vec<_> = iter
        .next()
        .unwrap()
        .iter()
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();

            let a = a.parse::<u64>().unwrap();
            let b = b.parse::<u64>().unwrap();

            assert!(a <= b);
            a..=b
        })
        .collect();

    let range_closure = base::fixpoint(ranges, merge_ranges);

    let ids: Vec<_> = iter
        .next()
        .unwrap()
        .iter()
        .map(|s| s.parse::<u64>().unwrap())
        .collect();

    let mut n = 0;
    for i in ids {
        let opt = range_closure.iter().find(|&x| x.contains(&i));
        if opt.is_some() {
            n = n + 1;
        }
    }

    println!("{:#?}", n);

    println!(
        "{:#?}",
        range_closure
            .iter()
            .fold(0, |acc, range| acc + range.end() - range.start() + 1)
    );
}
