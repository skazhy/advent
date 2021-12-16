// Advent of Code 2021, day 5: Trick Shot
// https://adventofcode.com/2021/day/17

use std::cmp;

fn max_height_for_velocity(
    upper_bound: (i32, i32),
    lower_bound: (i32, i32),
    mut vx: i32,
    mut vy: i32,
) -> Option<i32> {
    let mut x = 0;
    let mut y = 0;
    let mut max_y = 0;

    while x <= lower_bound.0 && y >= lower_bound.1 {
        x += vx;
        y += vy;
        vy -= 1;
        vx = cmp::max(0, vx - 1);
        max_y = cmp::max(max_y, y);

        if lower_bound.0 >= x && x >= upper_bound.0 && lower_bound.1 <= y && y <= upper_bound.1 {
            return Some(max_y);
        }
    }
    None
}

fn parse_bound(bound: &str) -> Vec<i32> {
    bound
        .split_once("=")
        .unwrap()
        .1
        .split("..")
        .flat_map(|c| c.parse::<i32>().ok())
        .collect::<Vec<i32>>()
}

pub fn run(input: &str) {
    let bounds = input
        .replace(",", "")
        .split(' ')
        .skip(2)
        .flat_map(|b| parse_bound(b))
        .collect::<Vec<i32>>();

    let upper_bound = (bounds[0], bounds[3]);
    let lower_bound = (bounds[1], bounds[2]);

    let mut max_y = 0;
    let mut count = 0;

    for vx in 0..lower_bound.0 + 1 {
        for vy in lower_bound.1..-lower_bound.1 + 1 {
            if let Some(local_max) = max_height_for_velocity(upper_bound, lower_bound, vx, vy) {
                count += 1;
                max_y = cmp::max(max_y, local_max);
            }
        }
    }

    println!("{:?}", max_y);
    println!("{:?}", count);
}
