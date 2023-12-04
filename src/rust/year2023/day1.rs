// Advent of Code 2023, day 1: Trebuchet?!
// https://adventofcode.com/2023/day/1

struct Pair {
    first: Option<char>,
    last: Option<char>,
}

impl Pair {
    pub fn empty() -> Self {
        Pair {
            first: None,
            last: None,
        }
    }
    pub fn push(&mut self, v: char) {
        if self.first.is_none() {
            self.first = Some(v)
        } else {
            self.last = Some(v)
        }
    }
    pub fn usize(self) -> usize {
        let mut s = String::from(self.first.unwrap());
        s.push(self.last.unwrap_or(self.first.unwrap()));
        s.parse::<usize>().unwrap()
    }
}

fn join_digits(input: &str) -> usize {
    let mut p = Pair::empty();
    for c in input.chars().filter(|x| x.is_ascii_digit()) {
        p.push(c);
    }
    p.usize()
}

fn join_digits2(input: &str) -> usize {
    let mut pair = Pair::empty();
    let mut i = 0;

    while i < input.len() {
        if (input[i..]).starts_with("one") {
            pair.push('1');
            i += 2;
        } else if (input[i..]).starts_with("two") {
            pair.push('2');
            i += 2;
        } else if (input[i..]).starts_with("three") {
            pair.push('3');
            i += 4;
        } else if (input[i..]).starts_with("four") {
            pair.push('4');
            i += 4;
        } else if (input[i..]).starts_with("five") {
            pair.push('5');
            i += 3;
        } else if (input[i..]).starts_with("six") {
            pair.push('6');
            i += 3;
        } else if (input[i..]).starts_with("seven") {
            pair.push('7');
            i += 4;
        } else if (input[i..]).starts_with("eight") {
            pair.push('8');
            i += 4;
        } else if (input[i..]).starts_with("nine") {
            pair.push('9');
            i += 3;
        } else if let Some(h) = input[i..i + 1].chars().next() {
            if h.is_ascii_digit() {
                pair.push(h);
            }
            i += 1;
        }
    }
    pair.usize()
}

pub fn run(input: &str) {
    println!("{}", input.lines().fold(0, |acc, r| acc + join_digits(r)));
    println!("{}", input.lines().fold(0, |acc, r| acc + join_digits2(r)));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn digit_join() {
        assert_eq!(join_digits("9986fmfqhdmq8"), 98);
        assert_eq!(join_digits("9986fmfqhdmq8"), 98);
    }
    #[test]
    fn digit_join2() {
        assert_eq!(join_digits2("two1nine"), 29);
        assert_eq!(join_digits2("abcone2threexyz"), 13);
        assert_eq!(join_digits2("xtwone3four"), 24);
        assert_eq!(join_digits2("4nineeightseven2"), 42);
        assert_eq!(join_digits2("zoneight234"), 14);
        assert_eq!(join_digits2("7pqrstsixteen"), 76);
        assert_eq!(join_digits2("37dqpbmqxssvznrzp2nvzcvlnsdoneightq"), 38);
    }
}
