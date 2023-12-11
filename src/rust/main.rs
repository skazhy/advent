// This file is automatically generated from scripts/templates/rust_main.txt
// whenever a new puzzle source is added.

mod bench;
mod intcode;
mod year2015;
mod year2016;
mod year2017;
mod year2019;
mod year2021;
mod year2022;
mod year2023;

use std::env;
use std::fs;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let year = &args[1];
    let day = &args[2];
    let input = fs::read_to_string(format!("resources/{}/day{}.txt", year, day))?;

    let puzzle = match (year.as_ref(), day.as_ref()) {
        ("2015", "1") => year2015::day1::run,
        ("2016", "1") => year2016::day1::run,
        ("2016", "2") => year2016::day2::run,
        ("2016", "3") => year2016::day3::run,
        ("2016", "7") => year2016::day7::run,
        ("2017", "2") => year2017::day2::run,
        ("2019", "2") => year2019::day2::run,
        ("2021", "17") => year2021::day17::run,
        ("2022", "1") => year2022::day1::run,
        ("2023", "1") => year2023::day1::run,
        ("2023", "11") => year2023::day11::run,
        ("2023", "4") => year2023::day4::run,
        ("2023", "6") => year2023::day6::run,
        ("2023", "8") => year2023::day8::run,
        _ => panic!("No source for {}/{}", year, day),
    };
    puzzle(input.trim());
    Ok(())
}
