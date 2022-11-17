// Advent of Code 2017, day 2: Corruption Checksum
// https://adventofcode.com/2017/day/2

use itertools::Itertools;

fn row_checksum(row: Vec<i32>) -> i32 {
    row.last().unwrap() - row.first().unwrap()
}

fn evenly_divisible(row: Vec<i32>) -> i32 {
    let mut res = 0;
    for (a, b) in row.into_iter().rev().tuple_combinations() {
        if a % b == 0 {
            res = a / b;
            break;
        }
    }
    res
}

pub fn run(input: &str) {
    let mut res1 = 0;
    let mut res2 = 0;

    for row in input.split('\n') {
        let mut parsed: Vec<i32> = row
            .split('\t')
            .map(|s| s.trim().parse::<i32>().unwrap())
            .collect();
        parsed.sort();

        res1 += row_checksum(parsed.clone());
        res2 += evenly_divisible(parsed.clone());
    }

    println!("{}", res1);
    println!("{}", res2);
}
