// Advent of Code 2016, day 2: Bathroom Security
// https://adventofcode.com/2016/day/2

use std::collections::HashMap;

fn update_coords(coords: (i32, i32), mov: &char) -> (i32, i32) {
    match mov {
        'U' => (coords.0, coords.1 - 1),
        'D' => (coords.0, coords.1 + 1),
        'L' => (coords.0 - 1, coords.1),
        _ => (coords.0 + 1, coords.1),
    }
}

fn solve_with_keypad(input: &str, keypad: HashMap<(i32, i32), char>) -> String {
    let coords = *keypad
        .iter()
        .find_map(|(key, &val)| if val == '5' { Some(key) } else { None })
        .unwrap();

    input
        .split('\n')
        .scan(coords, |coords, digit| {
            *coords = digit.chars().fold(*coords, |crds, mov| {
                let new_coords = update_coords(crds, &mov);
                if keypad.contains_key(&new_coords) {
                    new_coords
                } else {
                    crds
                }
            });
            keypad.get(coords)
        })
        .collect::<String>()
}

fn puzzle1(input: &str) -> String {
    solve_with_keypad(
        input,
        HashMap::from([
            ((0, 0), '1'),
            ((1, 0), '2'),
            ((2, 0), '3'),
            ((0, 1), '4'),
            ((1, 1), '5'),
            ((2, 1), '6'),
            ((0, 2), '7'),
            ((1, 2), '8'),
            ((2, 2), '9'),
        ]),
    )
}

fn puzzle2(input: &str) -> String {
    solve_with_keypad(
        input,
        HashMap::from([
            ((2, 0), '1'),
            ((1, 1), '2'),
            ((2, 1), '3'),
            ((3, 1), '4'),
            ((0, 2), '5'),
            ((1, 2), '6'),
            ((2, 2), '7'),
            ((3, 2), '8'),
            ((4, 2), '9'),
            ((1, 3), 'A'),
            ((2, 3), 'B'),
            ((3, 3), 'C'),
            ((2, 4), 'D'),
        ]),
    )
}

pub fn run(input: &str) {
    println!("{}", puzzle1(input));
    println!("{}", puzzle2(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn puzzle_1_example() {
        assert_eq!(puzzle1("ULL\nRRDDD\nLURDL\nUUUUD"), String::from("1985"));
    }

    #[test]
    fn puzzle_2_example() {
        assert_eq!(puzzle2("ULL\nRRDDD\nLURDL\nUUUUD"), String::from("5DB3"));
    }
}
