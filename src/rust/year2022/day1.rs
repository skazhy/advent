// Advent of Code 2022, day 1: Calorie Counting
// https://adventofcode.com/2022/day/1

pub fn run(input: &str) {
    let mut grouped: Vec<i32> = Vec::new();
    for cals in input
        .split('\n')
        .collect::<Vec<&str>>()
        .split(|c| c.is_empty())
    {
        grouped.push(
            cals.iter()
                .fold(0, |acc, c| acc + c.parse::<i32>().unwrap()),
        );
    }
    grouped.sort_by(|a, b| b.cmp(a));
    println!("{}", grouped.first().unwrap());
    println!("{}", grouped[0..3].iter().sum::<i32>())
}
