use std::collections::HashMap;
use std::num::ParseIntError;
use std::str::FromStr;
use thiserror::Error;

/// A valid input to Advent of Code 2024 Day 1.
/// ```
/// # use my_solutions::day1;
/// let from_str: day1::Input =
///     "3   4
///      4   3
///      2   5
///      1   3
///      3   9
///      3   3".parse().unwrap();
/// let from_vecs = day1::Input::new(
///     vec![3, 4, 2, 1, 3, 3],
///     vec![4, 3, 5, 3, 9, 3],
/// ).unwrap();
/// assert_eq!(from_str, from_vecs)
/// ```
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Input {
    left_list: Vec<u32>,
    right_list: Vec<u32>,
}

#[derive(Error, Debug)]
pub enum InvalidInput {
    #[error("expected equal lengths, got len(left_list) = {0}, len(right_list) = {1}")]
    MismatchedLengths(usize, usize),
}

impl Input {
    /// Create an input from the two lists. The only requirement is that the lists are the same
    /// length.
    /// ```
    /// # use my_solutions::day1;
    /// let valid = day1::Input::new(vec![1, 2, 3], vec![4, 5, 6]);
    /// assert!(valid.is_ok());
    /// let bogus = day1::Input::new(vec![1, 2, 3], vec![4, 5]);
    /// assert!(bogus.is_err());
    /// ```
    pub fn new(left_list: Vec<u32>, right_list: Vec<u32>) -> Result<Self, InvalidInput> {
        let (left_len, right_len) = (left_list.len(), right_list.len());
        if left_len == right_len {
            Ok(Self {
                left_list,
                right_list,
            })
        } else {
            Err(InvalidInput::MismatchedLengths(left_len, right_len))
        }
    }
}

#[derive(Error, Debug)]
pub enum ParseInputError {
    #[error("expected 2 tokens per line, got {} on line {}: {:?}", .1.len(), .0, .1)]
    WrongNumberOfTokens(usize, Vec<String>),
    #[error("expected a u32 integer on line {0}, got {1:?}")]
    ExpectedU32(usize, String, #[source] ParseIntError),
}

impl FromStr for Input {
    type Err = ParseInputError;

    /// Parse an input from a string, in the format of an Advent of Code input file.
    fn from_str(input_str: &str) -> Result<Self, Self::Err> {
        let mut input = Self {
            left_list: Vec::new(),
            right_list: Vec::new(),
        };
        for (line, line_num) in input_str.lines().zip(1..) {
            let tokens: Vec<&str> = line.split_whitespace().collect();
            let [left, right] = tokens.try_into().map_err(|tokens: Vec<_>| {
                Self::Err::WrongNumberOfTokens(
                    line_num,
                    tokens.into_iter().map(str::to_owned).collect(),
                )
            })?;

            macro_rules! parse_and_push {
                ($num_str:ident, $list:expr) => {
                    $list.push($num_str.parse().map_err(|source| {
                        Self::Err::ExpectedU32(line_num, $num_str.into(), source)
                    })?);
                };
            }
            parse_and_push!(left, input.left_list);
            parse_and_push!(right, input.right_list);
        }
        Ok(input)
    }
}

#[test]
fn example_input() {
    let input = Input::new(vec![3, 4, 2, 1, 3, 3], vec![4, 3, 5, 3, 9, 3]).unwrap();
    assert_eq!(11, part1(input.clone()));
    assert_eq!(31, part2(input))
}

pub fn part1(mut input: Input) -> u32 {
    input.left_list.sort();
    input.right_list.sort();
    let mut total_distance = 0;
    for (left, right) in input.left_list.into_iter().zip(input.right_list) {
        total_distance += u32::abs_diff(left, right);
    }
    total_distance
}

pub fn part2(input: Input) -> u64 {
    // We could start by sorting as before and then iterate as if merging.
    // Using a HashMap achieves O(N)-ish time complexity instead of O(N log N), however.
    let mut right_list_counts = HashMap::new();
    for location_id in input.right_list {
        right_list_counts
            .entry(location_id)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }

    let mut similarity_score = 0;
    for location_id in input.left_list {
        if let Some(count) = right_list_counts.get(&location_id) {
            similarity_score += u64::from(location_id * count);
        }
    }
    similarity_score
}
