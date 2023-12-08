// Advent of Code 2023, day 8: Haunted Wasteland
// https://adventofcode.com/2023/day/8

use num::integer::lcm;
use std::collections::HashMap;

fn puzzle1(dirs: &str, grid: HashMap<&str, (&str, &str)>) -> usize {
    let mut i = 0;
    let mut di = dirs.chars().cycle();
    let mut f = "AAA";
    loop {
        if f == "ZZZ" {
            return i;
        } else {
            i += 1;
            if di.next().unwrap() == 'L' {
                f = grid.get(f).unwrap().0;
            } else {
                f = grid.get(f).unwrap().1;
            }
        }
    }
}

fn puzzle2(dirs: &str, grid: HashMap<&str, (&str, &str)>) -> usize {
    let mut i = 0;
    let mut m = 1;
    let mut di = dirs.chars().cycle();
    let mut fs: Vec<&str> = grid.keys().filter(|x| x.ends_with('A')).copied().collect();

    let mut rem: Option<usize> = None;

    loop {
        if fs.is_empty() {
            return m;
        }

        let d = di.next().unwrap();

        for j in 0..fs.len() {
            if fs[j].ends_with('Z') {
                m = lcm(m, i);
                rem = Some(j);
            } else {
                let f = grid.get(fs[j]).unwrap();
                if d == 'L' {
                    fs[j] = f.0;
                } else {
                    fs[j] = f.1;
                }
            }
        }
        i += 1;

        if let Some(j) = rem {
            fs.remove(j);
            rem = None;
        }
    }
}

pub fn run(input: &str) {
    let mut i = input.split('\n');
    let dirs = i.next().unwrap();

    let graph: HashMap<&str, (&str, &str)> = i
        .skip(1)
        .map(|a| (&a[0..3], (&a[7..10], &a[12..15])))
        .collect();

    println!("{:?}", puzzle1(dirs, graph.clone()));
    println!("{:?}", puzzle2(dirs, graph.clone()));
}
