// Advent of Code 2023, day 16: The Floor Will Be Lava
// https://adventofcode.com/2023/day/16

use std::collections::{HashSet, VecDeque};

#[derive(Debug, Clone, Copy, PartialEq)]
struct Beam {
    pub coords: (i8, i8),
    pub dx: i8,
    pub dy: i8,
}

impl Beam {
    /// Advances beam in-place, returns an optional
    /// new beam if splitter is encountered.
    fn advance(&mut self, d: char) -> Option<Beam> {
        let mut split_beam = None;
        match d {
            '/' => {
                (self.dx, self.dy) = (-self.dy, -self.dx);
            }
            '\\' => {
                (self.dx, self.dy) = (self.dy, self.dx);
            }
            // Split vertical beam into 2 horizontal beams
            '-' if self.dx == 0 => {
                (self.dx, self.dy) = (-1, 0);
                split_beam = Some(Beam {
                    coords: (self.coords.0 + 1, self.coords.1),
                    dx: 1,
                    dy: 0,
                });
            }
            // Split horizontal beam into 2 vertical beams
            '|' if self.dy == 0 => {
                (self.dx, self.dy) = (0, -1);
                split_beam = Some(Beam {
                    coords: (self.coords.0, self.coords.1 + 1),
                    dx: 0,
                    dy: 1,
                });
            }
            _ => (),
        }
        self.coords.0 += self.dx;
        self.coords.1 += self.dy;

        split_beam
    }
}

fn energized_tile_count(grid: &Vec<Vec<char>>, initial: Beam) -> usize {
    let gx: i8 = grid[0].len().try_into().unwrap();
    let gy: i8 = grid.len().try_into().unwrap();

    let in_bounds =
        |b: Beam| b.coords.0 >= 0 && b.coords.1 >= 0 && b.coords.1 < gy && b.coords.0 < gx;

    let mut visited: HashSet<((i8, i8), (i8, i8))> = HashSet::new();
    let mut vis2: HashSet<(i8, i8)> = HashSet::new();

    let mut beams = VecDeque::from([initial]);

    while let Some(mut b) = beams.pop_front() {
        visited.insert((b.coords, (b.dx, b.dy)));
        vis2.insert(b.coords);
        if let Some(split_beam) = b.advance(grid[b.coords.1 as usize][b.coords.0 as usize]) {
            if in_bounds(split_beam)
                && !visited.contains(&(split_beam.coords, (split_beam.dx, split_beam.dy)))
            {
                beams.push_back(split_beam);
            }
        }

        if in_bounds(b) && !visited.contains(&(b.coords, (b.dx, b.dy))) {
            beams.push_back(b);
        }
    }
    vis2.len()
}

pub fn run(input: &str) {
    let grid: Vec<Vec<char>> = input.lines().map(|l| l.chars().collect()).collect();

    // Part 1
    println!(
        "{}",
        energized_tile_count(
            &grid,
            Beam {
                coords: (0, 0),
                dx: 1,
                dy: 0
            }
        )
    );

    // Part 2
    let gx: i8 = grid[0].len().try_into().unwrap();
    let gy: i8 = grid.len().try_into().unwrap();

    let mut emax = 0;
    for i in 0..gy {
        emax = std::cmp::max(
            emax,
            energized_tile_count(
                &grid,
                Beam {
                    coords: (0, i),
                    dx: 1,
                    dy: 0,
                },
            ),
        );
        emax = std::cmp::max(
            emax,
            energized_tile_count(
                &grid,
                Beam {
                    coords: (gx - 1, i),
                    dx: -1,
                    dy: 0,
                },
            ),
        );
    }
    for i in 0..gx {
        emax = std::cmp::max(
            emax,
            energized_tile_count(
                &grid,
                Beam {
                    coords: (i, 0),
                    dx: 0,
                    dy: 1,
                },
            ),
        );
        emax = std::cmp::max(
            emax,
            energized_tile_count(
                &grid,
                Beam {
                    coords: (i, gy - 1),
                    dx: 0,
                    dy: -1,
                },
            ),
        );
    }
    println!("{}", emax);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn beam_advance() {
        let mut b = Beam {
            coords: (0, 0),
            dx: 1,
            dy: 0,
        };
        assert_eq!(b.advance('/'), None);
        assert_eq!(b.coords, (0, -1));
        assert_eq!(b.advance('/'), None);
        assert_eq!(b.coords, (1, -1));
    }

    #[test]
    fn beam_advance_split_h() {
        let mut b = Beam {
            coords: (0, 0),
            dx: 1,
            dy: 0,
        };
        assert_eq!(
            b.advance('|'),
            Some(Beam {
                coords: (0, 1),
                dx: 0,
                dy: 1
            })
        );
        assert_eq!(
            b,
            Beam {
                coords: (0, -1),
                dx: 0,
                dy: -1
            }
        );
    }
    #[test]
    fn beam_split_v() {
        let mut b = Beam {
            coords: (0, 0),
            dx: 0,
            dy: 1,
        };

        assert_eq!(
            b.advance('-'),
            Some(Beam {
                coords: (1, 0),
                dx: 1,
                dy: 0
            })
        );
        assert_eq!(
            b,
            Beam {
                coords: (-1, 0),
                dx: -1,
                dy: 0
            }
        );
    }
}
