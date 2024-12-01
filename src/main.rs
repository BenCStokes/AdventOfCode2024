use std::io;

use my_solutions::day1;

fn main() -> anyhow::Result<()> {
    let my_input: day1::Input = io::read_to_string(io::stdin())?.parse()?;
    println!("Part 1: {}", day1::part1(my_input.clone()));
    println!("Part 2: {}", day1::part2(my_input));
    Ok(())
}
