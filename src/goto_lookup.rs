use std::env;
use std::fs;
use std::path::Path;

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

pub static GOT_TO_FILE: &str = "/.got_to";

pub fn home() -> String {
    match env::home_dir() {
        Some(path) => path.to_str().unwrap().to_string(),
        None => panic!("home directory not found!"),
    }
}

pub fn lines() -> Vec<String> {
    return match fs::read_to_string(home() + GOT_TO_FILE) {
        Ok(content) => Vec::from_iter(content.split("\n").into_iter().map(|it| it.to_string())),
        Err(e) => {
            println!("Could not read {}: {}", GOT_TO_FILE, e);
            vec![]
        }
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

fn relative_to_abs(path: String, path_separator: &str) -> String {
    return path
        .split(path_separator)
        .fold(&mut Vec::new(), |acc: &mut Vec<String>, it| match it {
            "." => acc,
            ".." => {
                acc.pop();
                acc
            }
            _ => {
                acc.push(it.to_string());
                acc
            }
        })
        .join(path_separator)
        .to_string();
}

pub fn find_dir(needle: &String, cwd: String, path_separator: &str) -> Option<String> {
    let parent = cwd.to_string() + path_separator + "..";
    let full_path = match needle.as_str() {
        ".." => parent,
        "." => cwd.to_string(),
        x if x.starts_with(vec!["..", path_separator].join("").as_str()) => parent + &x[2..],
        x if x.starts_with(vec![".", path_separator].join("").as_str()) => cwd + &x[1..],
        x if std::path::Path::new(x).is_relative() => cwd + path_separator + &x.to_string(),
        x => x.to_string(),
    };
    let abs_needle = relative_to_abs(full_path, path_separator);
    let abs_needle = 
        abs_needle.strip_suffix(path_separator).unwrap_or(abs_needle.as_str());

    return if Path::new(abs_needle).exists() {
        Some(abs_needle.to_string())
    } else {
        None
    }
}

pub fn find(query: Query, lines: &[String]) -> Option<String> {
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
            // .map_or(vec![], |value| vec![value])
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
            if needle.len() == 0 {
                return lines.to_vec();
            }
            let prepare = match ignore_case {
                true => |it: String| it.to_lowercase(),
                false => |it: String| it,
            };
            let needle = prepare(needle);
            lines
                .iter()
                .filter(|it| {
                    prepare(it.to_string()).contains(&needle)
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

            let apply_needle = |acc: Option<String>, needle: String| -> Option<String> {
                acc.and_then(|acc| {
                    acc.find(&needle)
                        .and_then(|idx| Some(acc[idx + needle.len()..].to_string()))
                })
            };

            let check_needles = |line: &&String| -> bool {
                needles
                    .iter()
                    .map(|it| prepare(it.to_string()))
                    .fold(Some(prepare(line.to_string())), apply_needle)
                    .is_some()
            };

            lines.iter().filter(check_needles).cloned().collect()
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

    #[test]
    fn test_filter_single() {
        assert_eq!(
            filter(
                Query::Single {
                    ignore_case: false,
                    needle: "lo".to_string()
                },
                &[
                    "tralala".to_string(),
                    "hallo welt".to_string(),
                    "hallo".to_string(),
                    "uwu".to_string()
                ]
            ),
            vec!["hallo welt".to_string(), "hallo".to_string()]
        );
    }

    #[test]
    fn test_filter_single_ignore_case() {
        assert_eq!(
            filter(
                Query::Single {
                    ignore_case: true,
                    needle: "Lo".to_string()
                },
                &[
                    "tralala".to_string(),
                    "hallO welt".to_string(),
                    "hallo".to_string(),
                    "uwu".to_string()
                ]
            ),
            vec!["hallO welt".to_string(), "hallo".to_string()]
        );
    }
}
