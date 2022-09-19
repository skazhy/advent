// Advent of Code 2015, day 1: Not Quite Lisp
// https://adventofcode.com/2015/day/1

fn floor_direction(i: u8) -> i32 {
    if i == 40 {
        1
    } else {
        -1
    }
}

fn puzzle1(input: &str) -> i32 {
    let mut floor = 0;
    for i in input.bytes() {
        floor += floor_direction(i);
    }
    floor
}

fn puzzle2(input: &str) -> i32 {
    let mut floor = 0;
    let mut idx = 0;
    for i in input.bytes() {
        idx += 1;
        floor += floor_direction(i);
        if floor < 0 {
            break;
        }
    }
    idx
}

pub fn run(input: &str) {
    println!("{:?}", puzzle1(input));
    println!("{:?}", puzzle2(input));
}
