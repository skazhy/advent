mod year2021;

use std::env;
use std::fs;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let year = &args[1];
    let day = &args[2];
    let input = fs::read_to_string(format!("resources/{}/day{}.txt", year, day))?;

    let puzzle = match (year.as_ref(), day.as_ref()) {
        ("2021", "17") => year2021::day17::run,
        _ => panic!("No source for {}/{}", year, day),
    };
    puzzle(input.trim());
    Ok(())
}
