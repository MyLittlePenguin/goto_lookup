use std::env;
use std::fs;

pub enum Query {
    Single {
        ignore_case: bool,
        needle: String,
    },
    Multi {
        ignore_case: bool,
        needles: Vec<String>,
    },
}

type Preparator = fn(String) -> String;

static GOT_TO_FILE: &str = "/.got_to";

pub fn home() -> String {
    match env::home_dir() {
        Some(path) => path.to_str().unwrap().to_string(),
        None => panic!("home directory not found!"),
    }
}

pub fn lines() -> Vec<String> {
    return match fs::read_to_string(home() + GOT_TO_FILE) {
        Ok(content) => Vec::from_iter(content.split("\n").into_iter().map(|it| it.to_string())),
        Err(_) => vec![],
    };
}

fn find_with(f: impl Fn(String) -> bool, list: &[String]) -> Option<String> {
    match list.len() {
        0 => None,
        _ if f(list[0].clone()) => Some(list[0].clone()),
        _ => find_with(f, &list[1..]),
    }
}

fn find_perfect(prepare: Preparator, needle: String, list: &[String]) -> Option<String> {
    let prepared_needle = prepare(needle);
    find_with(|it| prepare(it) == prepared_needle, list)
}

fn find_end(prepare: Preparator, needle: String, list: &[String]) -> Option<String> {
    let prepared_needle = prepare(needle);
    find_with(|it| prepare(it).ends_with(&prepared_needle), list)
}

fn find_some(prepare: Preparator, needle: String, list: &[String]) -> Option<String> {
    let prepared_needle = prepare(needle);
    find_with(|it| prepare(it).contains(&prepared_needle), list)
}

pub fn find(query: Query, lines: &[String]) -> Vec<String> {
    match query {
        Query::Single {
            ignore_case,
            needle,
        } => {
            let prepare = match ignore_case {
                true => |it: String| it.to_lowercase(),
                false => |it: String| it,
            };
            find_perfect(prepare, needle.clone(), lines)
                .or_else(|| find_end(prepare, needle.clone(), lines))
                .or_else(|| find_some(prepare, needle.clone(), lines))
                .map_or(vec![], |value| vec![value])
        }
        Query::Multi {
            ignore_case,
            needles,
        } => find(
            Query::Single {
                ignore_case,
                needle: needles[needles.len() - 1].clone(),
            },
            &filter(
                Query::Multi {
                    ignore_case,
                    needles,
                },
                lines,
            )[..],
        ),
    }
}

pub fn filter(query: Query, lines: &[String]) -> Vec<String> {
    match query {
        Query::Single {
            ignore_case,
            needle,
        } => {
            let needle = needle.to_lowercase();
            lines
                .iter()
                .filter(|it| {
                    if ignore_case {
                        it.to_lowercase().contains(&needle)
                    } else {
                        it.contains(&needle)
                    }
                })
                .cloned()
                .collect()
        }
        Query::Multi {
            ignore_case,
            needles,
        } => {
            let prepare = match ignore_case {
                true => |it: String| it.to_lowercase(),
                false => |it: String| it,
            };
            // something with left fold
            todo!()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_with_equal() {
        assert_eq!(
            find_with(
                |it| it == "hallo",
                &[
                    "tralala".to_string(),
                    "hallo welt".to_string(),
                    "hallo".to_string(),
                    "uwu".to_string()
                ]
            ),
            Some("hallo".to_string())
        );
    }

    #[test]
    fn test_find_perfect() {
        assert_eq!(
            find_perfect(
                |it| it.to_string(),
                "hallo".to_string(),
                &[
                    "tralala".to_string(),
                    "hallo welt".to_string(),
                    "hallo".to_string(),
                    "uwu".to_string()
                ]
            ),
            Some("hallo".to_string())
        );
    }

    #[test]
    fn test_find_end() {
        assert_eq!(
            find_end(
                |it| it.to_string(),
                "lo".to_string(),
                &[
                    "tralala".to_string(),
                    "hallo welt".to_string(),
                    "hallo".to_string(),
                    "uwu".to_string()
                ]
            ),
            Some("hallo".to_string())
        );
    }

    #[test]
    fn test_find_some() {
        assert_eq!(
            find_some(
                |it| it.to_string(),
                "lo".to_string(),
                &[
                    "tralala".to_string(),
                    "hallo welt".to_string(),
                    "hallo".to_string(),
                    "uwu".to_string()
                ]
            ),
            Some("hallo welt".to_string())
        );
    }
}
