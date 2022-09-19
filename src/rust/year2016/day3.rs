// Advent of Code 2016, day 3: Squares With Three Sides
// https://adventofcode.com/2016/day/3

fn valid_triangle(input: &[i32]) -> bool {
    let mut sides = input.to_owned();
    sides.sort();
    sides.iter().take(2).sum::<i32>() > *sides.last().unwrap()
}

fn parse_row(input: &str) -> Vec<i32> {
    input.split_whitespace()
        .map(|d| d.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
}

fn puzzle1(input: &str) -> usize {
    input
        .split('\n')
        .map(|r| parse_row(r))
        .filter(|t| valid_triangle(t))
        .count()
}

// 1237 -> too low

fn puzzle2(input: &str) -> usize {
    let mut row_iter = input.split('\n').map(|r| parse_row(r));
    let mut valid = 0;

    while let Some(a) = row_iter.next() {
        let b = row_iter.next().unwrap().clone();
        let c = row_iter.next().unwrap().clone();

        for i in 0..3 {
            if valid_triangle(&[
                *a.get(i).unwrap(),
                *b.get(i).unwrap(),
                *c.get(i).unwrap(),
            ]) {
                valid += 1;
            }
        }
    }
    valid
}

pub fn run(input: &str) {
    println!("{:?}", puzzle1(input));
    println!("{:?}", puzzle2(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn puzzle_examples() {
        assert_eq!(valid_triangle(&vec![5, 10, 25]), false)
    }
}
