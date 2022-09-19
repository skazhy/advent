// Advent of Code 2016, day 1: No Time for a Taxicab
// https://adventofcode.com/2016/day/1

use std::collections::HashSet;

fn manhattan_distance(input: &str, stop_on_visited: bool) -> i32 {
    let mut dir: (i32, i32) = (0, 1);
    let mut coords: (i32, i32) = (0, 0);

    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    visited.insert((0, 0));

    'outer: for coord in input.split(", ") {
        let (turn, b) = coord.split_at(1);
        let blocks: i32 = b.parse().unwrap();

        if turn == "R" {
            dir = (dir.1, if dir.1 == 0 { -dir.0 } else { 0 });
        } else {
            dir = (if dir.0 == 0 { -dir.1 } else { 0 }, dir.0);
        }

        if stop_on_visited {
            for _ in 1..blocks + 1 {
                coords = (coords.0 + dir.0, coords.1 + dir.1);
                if visited.contains(&coords) {
                    break 'outer;
                } else {
                    visited.insert(coords);
                }
            }
        } else {
            coords = (coords.0 + blocks * dir.0, coords.1 + blocks * dir.1);
        }
    }
    i32::abs(coords.0) + i32::abs(coords.1)
}

pub fn run(input: &str) {
    println!("{:?}", manhattan_distance(input, false));
    println!("{:?}", manhattan_distance(input, true));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn puzzle_1_examples() {
        assert_eq!(manhattan_distance("R2, L3", false), 5);
        assert_eq!(manhattan_distance("R2, R2, R2", false), 2);
        assert_eq!(manhattan_distance("R5, L5, R5, R3", false), 12);
    }

    #[test]
    fn puzzle_2_examples() {
        assert_eq!(manhattan_distance("R8, R4, R4, R8", true), 4);
    }
}
