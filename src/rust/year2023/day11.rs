// Advent of Code 2023, day 11: Cosmic Expansion
// https://adventofcode.com/2023/day/11

use itertools::Itertools;
use std::collections::HashSet;

pub fn run(input: &str) {
    // Build a hash set of all galaxies & a set of blank rows and columns
    // that will be added when calculating the Manhattan distance between
    // galaxies.
    let mut galaxies: HashSet<(usize, usize)> = HashSet::new();
    let mut row_count: usize = 0;
    let mut col_count: usize = 0;

    let mut filled_cols: HashSet<usize> = HashSet::new();
    let mut filled_rows: HashSet<usize> = HashSet::new();

    for (y, row) in input.lines().enumerate() {
        if y == 0 {
            row_count = row.len();
        }
        for (x, c) in row.chars().enumerate() {
            if c == '#' {
                galaxies.insert((x, y));
                filled_cols.insert(x);
                filled_rows.insert(y);
            }
        }
        col_count += 1;
    }

    let blank_cols: HashSet<usize> = HashSet::from_iter(0..col_count)
        .difference(&filled_cols)
        .copied()
        .collect();
    let blank_rows: HashSet<usize> = HashSet::from_iter(0..row_count)
        .difference(&filled_rows)
        .copied()
        .collect();

    let mut p1: usize = 0;
    let mut p2: usize = 0;

    for c in galaxies.iter().combinations(2) {
        let a = c[0];
        let b = c[1];

        let (from_x, to_x) = if a.0 < b.0 { (a.0, b.0) } else { (b.0, a.0) };
        let (from_y, to_y) = if a.1 < b.1 { (a.1, b.1) } else { (b.1, a.1) };
        let manhattan_dist = to_x - from_x + to_y - from_y;
        let extra_steps = blank_cols
            .iter()
            .filter(|x| from_x < **x && **x < to_x)
            .count()
            + blank_rows
                .iter()
                .filter(|y| from_y < **y && **y < to_y)
                .count();

        p1 += manhattan_dist + extra_steps;
        p2 += manhattan_dist + extra_steps * 999999;
    }
    println!("{:?}", p1);
    println!("{:?}", p2);
}
