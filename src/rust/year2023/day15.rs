// Advent of Code 2023, day 15: Lens Library
// https://adventofcode.com/2023/day/15

use std::collections::HashMap;

fn encode_step(acc: usize, x: char) -> usize {
    ((acc + x as usize) * 17) % 256
}

fn encode(input: &str) -> usize {
    input.chars().fold(0, encode_step)
}

pub fn run(input: &str) {
    // Part 1
    println!("{:?}", input.split(',').map(encode).sum::<usize>());

    // Part 2
    let mut boxes: HashMap<usize, Vec<(String, usize)>> = HashMap::new();

    for item in input.split(',') {
        let (label, args): (Vec<char>, Vec<char>) =
            item.chars().partition(|x| x.is_ascii_alphabetic());
        let encoded = label.clone().into_iter().fold(0, encode_step);
        let l: String = label.iter().collect();

        match args[..] {
            ['=', x] => {
                let fl = (x as usize) - 48;
                boxes
                    .entry(encoded)
                    .and_modify(|v| {
                        let mut found = false;
                        for i in &mut *v {
                            if i.0 == l {
                                i.1 = fl;
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            v.push((label.iter().collect(), fl))
                        }
                    })
                    .or_insert(vec![(label.iter().collect(), fl)]);
            }
            ['-'] => {
                boxes.entry(encoded).and_modify(|v| {
                    *v = v
                        .iter_mut()
                        .filter(|x| x.0 != l)
                        .map(|x| x.clone())
                        .collect::<Vec<(String, usize)>>()
                });
            }
            _ => continue,
        }
    }

    let mut s: usize = 0;
    for (box_idx, lenses) in &boxes {
        if !lenses.is_empty() {
            s += lenses
                .iter()
                .enumerate()
                .map(|(idx, (_, fl))| (box_idx + 1) * (idx + 1) * fl)
                .sum::<usize>();
        }
    }
    println!("{}", s);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encoding_examples() {
        assert_eq!(encode("HASH"), 52);
        assert_eq!(encode("rn=1"), 30);
        assert_eq!(encode("rn"), 0);
    }
}
