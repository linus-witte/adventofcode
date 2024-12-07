mod input_reader;
use std::collections::HashMap;

#[derive(Debug)]
struct Game {
    id: u32,
    s: HashMap<String, Vec<u32>>,
}

fn build(line: &str) -> Result<Game, std::io::Error> {
    let vec: Vec<&str> = line.split(&[':', ',', ';'][..]).collect();
    let mut s: HashMap<String, Vec<u32>> = HashMap::new();
    let mut id: u32 = u32::MAX;

    for e in vec {
        let v: Vec<&str> = e.trim().split(' ').collect();
        let [l, r] = v.as_slice() else { todo!() };

        if l.eq_ignore_ascii_case("game") {
            id = r.parse().unwrap();
        } else {
            let n = l.trim().parse().unwrap();
            s.entry(r.to_string())
                .and_modify(|e| e.push(n))
                .or_insert(vec![n]);
        }
    }

    Ok(Game { id, s })
}

fn is_valid(game: &Game, capacities: &HashMap<&str, u32>) -> bool {
    game.s.clone().into_iter().all(|(key, val)| {
        capacities
            .get(&key[..])
            .map(|&c| val.into_iter().all(|v| v <= c))
            .unwrap_or(false)
    })
}

fn min_capacity(game: &Game) -> HashMap<&String, &u32> {
    let mut map = HashMap::new();
    for (color, numbers) in &game.s {
        map.insert(color, numbers.into_iter().max().unwrap());
    }
    map
}

fn main() -> Result<(), std::io::Error> {
    let games = input_reader::read_input("input", |line| build(&line[..]).unwrap())?;

    let cap = HashMap::from([("red", 12), ("green", 13), ("blue", 14)]);

    let sum: u32 = games
        .iter()
        .filter_map(|g| if is_valid(&g, &cap) { Some(g.id) } else { None })
        .sum();

    println!("Part 1: {sum}");

    let sum = games
        .iter()
        .map(|g| min_capacity(&g).values().fold(1, |acc, &a| acc * a))
        .fold(0, |acc, a| acc + a);

    println!("Part 2: {sum}");
    Ok(())
}
