pub fn run_program(mut ops: Vec<usize>) -> Vec<usize> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day2_examples() {
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
