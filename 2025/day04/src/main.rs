fn pick(map: Vec<Vec<bool>>) -> Vec<u32> {
    let height = map.len() as i32;
    let width = map[0].len() as i32;

    let mut sum = 0;
    let mut next_map = map.clone();

    for x in 0..width {
        for y in 0..height {
            if !map[y as usize][x as usize] {
                continue;
            }

            let neighbors: Vec<(i32, i32)> = base::ADJ8
                .iter()
                .filter_map(|&(d_y, d_x)| {
                    let n_y = y as i32 + d_y;
                    let n_x = x as i32 + d_x;

                    if n_x >= 0
                        && n_y >= 0
                        && n_x < width
                        && n_y < height
                        && map[n_y as usize][n_x as usize]
                    {
                        Some((n_x, n_y))
                    } else {
                        None
                    }
                })
                .collect();

            if neighbors.len() < 4 {
                next_map[y as usize][x as usize] = false;
                sum += 1;
            }
        }
    }

    if sum == 0 {
        return Vec::new();
    }

    let mut v = vec![sum];
    v.append(&mut pick(next_map));
    return v;
}

fn main() {
    let map: Vec<Vec<bool>> = base::read_input(|x| x.chars().map(|c| c == '@').collect()).unwrap();
    let res = pick(map);

    println!("{:#?}", res[0]);
    println!("{:#?}", res.iter().sum::<u32>());
}
