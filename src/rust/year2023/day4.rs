// Advent of Code 2023, day 4: Scratchcards
// https://adventofcode.com/2023/day/4

use std::collections::HashMap;
use std::collections::HashSet;

fn row_points(input: &str, offset: usize) -> usize {
    let i = input.split_whitespace().skip(2);
    let picked: HashSet<&str> = HashSet::from_iter(i.clone().take(offset));
    let winning = HashSet::from_iter(i.skip(offset));
    picked.intersection(&winning).count()
}

// Part 1

fn power_sum(points: Vec<usize>) -> i32 {
    let pow_base: i32 = 2;
    let mut sum = 0;
    for point in points {
        if point != 0 {
            sum += pow_base.pow(point as u32 - 1)
        }
    }
    sum
}

// Part 2
fn card_count(points: Vec<usize>) -> i32 {
    fn sublist_sum(points: &[(usize, &usize)], seen: &mut HashMap<usize, i32>) -> i32 {
        if let Some((idx, f)) = points.first() {
            if let Some(v) = seen.get(idx) {
                *v
            } else {
                let mut sum = 1;
                for i in 1..=(**f as i32) {
                    sum += sublist_sum(&points[(i as usize)..], seen);
                }
                seen.insert(*idx, sum);
                sum
            }
        } else {
            0
        }
    }

    let indexed_points: Vec<(usize, &usize)> = points.iter().enumerate().collect();
    let mut seen: HashMap<usize, i32> = HashMap::new();

    for i in 0..(indexed_points.len() + 1) {
        sublist_sum(&indexed_points[i..], &mut seen);
    }
    seen.values().sum()
}

pub fn run(input: &str) {
    let points: Vec<usize> = input.lines().map(|r| row_points(r, 10)).collect();

    println!("{:?}", power_sum(points.clone()));
    println!("{:?}", card_count(points));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        assert_eq!(
            row_points("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", 5),
            4
        );
        assert_eq!(
            row_points("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", 5),
            2
        );
        assert_eq!(
            row_points("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 5),
            2
        );
        assert_eq!(
            row_points("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", 5),
            1
        );
        assert_eq!(
            row_points("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", 5),
            0
        );
        assert_eq!(
            row_points("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", 5),
            0
        );
    }

    #[test]
    fn test_part2() {
        assert_eq!(card_count(vec![]), 0);
        assert_eq!(card_count(vec![1]), 1);
        assert_eq!(card_count(vec![4, 2, 2, 1, 0, 0]), 30);
    }
}
