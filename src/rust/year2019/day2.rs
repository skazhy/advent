// Advent of Code 2019, day 2: 1202 Program Alarm
// https://adventofcode.com/2019/day/2

fn run_program(mut ops: Vec<usize>) -> Vec<usize> {
    let mut idx = 0;
    while let Some(x) = ops.get(idx) {
        match x {
            1 => {
                let a = ops[ops[idx + 1]];
                let b = ops[ops[idx + 2]];
                let d = ops[idx + 3];
                ops[d] = a + b;
                idx += 4;
            }
            2 => {
                let a = ops[ops[idx + 1]];
                let b = ops[ops[idx + 2]];
                let d = ops[idx + 3];
                ops[d] = a * b;
                idx += 4;
            }
            _ => {
                break;
            }
        }
    }
    ops
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn puzzle_examples() {
        assert_eq!(
            run_program(Vec::from([1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50])),
            Vec::from([3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])
        );
        assert_eq!(
            run_program(Vec::from([2, 4, 4, 5, 99, 0])),
            Vec::from([2, 4, 4, 5, 99, 9801])
        );
        assert_eq!(
            run_program(Vec::from([1, 1, 1, 4, 99, 5, 6, 0, 99])),
            Vec::from([30, 1, 1, 4, 2, 5, 6, 0, 99])
        );
    }
}
