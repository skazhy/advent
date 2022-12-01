// Advent of Code 2019, day 2: 1202 Program Alarm
// https://adventofcode.com/2019/day/2

use crate::intcode::run_program;

fn run1(input: &str) {
    let mut ops = input
        .split(',')
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    ops[1] = 12;
    ops[2] = 2;
    let res1 = run_program(ops);
    println!("{:?}", res1[0]);
}

fn run2(input: &str) {
    let expected_out = 19690720;
    let mut ops = input
        .split(',')
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    ops[1] = 0;
    ops[2] = 0;
    let base = run_program(ops.clone())[0];

    ops[1] = 1;
    let noun_delta = run_program(ops.clone())[0] - base;
    let noun = (expected_out - base) / noun_delta;
    println!("{}", 100 * noun + (expected_out - base - noun * noun_delta));
}

pub fn run(input: &str) {
    run1(input);
    run2(input);
}
