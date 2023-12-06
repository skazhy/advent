// Advent of Code 2023, day 6: Wait For It
// https://adventofcode.com/2023/day/6

use itertools::Itertools;

fn winning_distance_count(time_record: (usize, usize)) -> usize {
    let mut first_winning = 0;
    let mut last_winning = 0;

    for i in 1..time_record.0 {
        if i * (time_record.0 - i) > time_record.1 {
            first_winning = i;
            break;
        }
    }
    for i in (first_winning..time_record.0 - 1).rev() {
        if i * (time_record.0 - i) > time_record.1 {
            last_winning = i;
            break;
        }
    }
    last_winning - first_winning + 1
}

// Part 1 - Parsing data as columns

fn parse_column(input: &str) -> impl Iterator<Item = usize> + '_ {
    input
        .split_whitespace()
        .skip(1)
        .map(|x| x.parse::<usize>().unwrap())
}

fn parse_as_columns(input: &str) -> Vec<(usize, usize)> {
    let mut rows = input.split_terminator("\n");

    let time = parse_column(rows.next().unwrap());
    let record = parse_column(rows.next().unwrap());
    time.zip(record).collect::<Vec<(usize, usize)>>()
}

// Part 2 - parsing data as rows

fn parse_row(input: &str) -> usize {
    input
        .split_whitespace()
        .skip(1)
        .join("")
        .parse::<usize>()
        .unwrap()
}

fn parse_as_rows(input: &str) -> (usize, usize) {
    let mut rows = input.split_terminator("\n");
    (
        parse_row(rows.next().unwrap()),
        parse_row(rows.next().unwrap()),
    )
}

pub fn run(input: &str) {
    println!(
        "{:?}",
        parse_as_columns(input)
            .into_iter()
            .map(winning_distance_count)
            .product::<usize>()
    );
    println!("{:?}", winning_distance_count(parse_as_rows(input)));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1_parsing() {
        assert_eq!(
            parse_as_columns("Time:      7  15   30\nDistance:  9  40  200"),
            vec![(7, 9), (15, 40), (30, 200)]
        );
    }
    #[test]
    fn p1_example() {
        assert_eq!(winning_distance_count((7, 9)), 4);
    }

    #[test]
    fn p2_parsing() {
        assert_eq!(
            parse_as_rows("Time:      7  15   30\nDistance:  9  40  200"),
            (71530, 940200)
        );
    }
}
