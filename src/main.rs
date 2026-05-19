use std::process;
use std::{
    env,
    fs::{self, File, remove_file},
    io::Write,
    path::Path,
};

use crate::errors::{Error, handle_error};
use crate::goto_lookup::{Query, filter, find, find_dir};

pub mod errors;
pub mod goto_lookup;

enum ActionType {
    PrintVersion,
    PrintHelp,
    Lookup,
    List,
    Clean,
    RemoveList,
    RemoveSingle,
    PrintOrphaned,
}

fn string(s: &str) -> String {
    s.to_string()
}

static GOT_TO_FILE: &str = "/.got_to";
static PATH_SEPARATOR: &str = std::path::MAIN_SEPARATOR_STR;

fn home() -> String {
    match env::home_dir() {
        Some(path) => path.to_str().unwrap().to_string(),
        // None => panic!("home directory not found!"),
        None => ".".to_string(),
    }
}

fn get_lookup_home() -> String {
    std::env::var("LOOKUP_HOME").unwrap_or_else(|_| home())
}

fn lines() -> Vec<String> {
    return match fs::read_to_string(get_lookup_home() + GOT_TO_FILE) {
        Ok(content) => Vec::from_iter(content.lines().into_iter().map(|it| it.to_string())),
        Err(_) => {
            write_new_lines(&"".to_string()).unwrap();
            vec![]
        }
    };
}

type ParsedArgState<'a> = (ActionType, bool, &'a mut Vec<String>);
type ArgSpec<'a, 'b, 'c> = (
    &'a str,
    &'b str,
    fn(ParsedArgState) -> ParsedArgState,
    &'c str,
);

const SPECS: [ArgSpec; 8] = [
    (
        "-i",
        "--ignore-case",
        |state| (state.0, true, state.2),
        "ignore case for finding the matches in the known locations",
    ),
    (
        "-I",
        "--no-ignore-case",
        |state| (state.0, false, state.2),
        "do not ignore case for finding the matches in the known locations",
    ),
    (
        "-l",
        "--list",
        |state| match state.0 {
            ActionType::RemoveSingle | ActionType::RemoveList => {
                (ActionType::RemoveList, state.1, state.2)
            }
            _ => (ActionType::List, state.1, state.2),
        },
        "list all matching locations",
    ),
    (
        "-v",
        "--version",
        |state| (ActionType::PrintVersion, state.1, state.2),
        "print version information",
    ),
    (
        "-h",
        "--help",
        |state| (ActionType::PrintHelp, state.1, state.2),
        "print help information",
    ),
    (
        "",
        "--clean",
        |state| (ActionType::Clean, state.1, state.2),
        "remove orphaned entries from the list of known locations",
    ),
    (
        "",
        "--show-orphaned",
        |state| (ActionType::PrintOrphaned, state.1, state.2),
        "show orphaned entries from the list of known locations",
    ),
    (
        "-d",
        "--delete",
        |state| match state.0 {
            ActionType::List => (ActionType::RemoveList, state.1, state.2),
            _ => (ActionType::RemoveSingle, state.1, state.2),
        },
        "delete entries found by the query",
    ),
];

fn parse_args(args: Vec<String>) -> (ActionType, Query) {
    let state: ParsedArgState = (ActionType::Lookup, false, &mut vec![]);
    let state = args[1..].iter().fold(state, |acc, it| -> ParsedArgState {
        match SPECS.iter().find(|spec| {
            (!spec.0.is_empty() && spec.0 == it) || (!spec.1.is_empty() && spec.1 == it)
        }) {
            Some(x) => x.2(acc),
            None => {
                acc.2.push(it.to_string());
                acc
            }
        }
    });
    match &state.2[..] {
        // maybe add Query::Empty to catch the zero needle case
        [] => (
            state.0,
            Query::Single {
                ignore_case: state.1,
                needle: string(""),
            },
        ),
        [a] => (
            state.0,
            Query::Single {
                ignore_case: state.1,
                needle: a.to_string(),
            },
        ),
        _ => (
            state.0,
            Query::Multi {
                ignore_case: state.1,
                needles: state.2.clone(),
            },
        ),
    }
}

fn print_help() {
    println!("Usage: {} [options] [query]", env!("CARGO_PKG_NAME"));
    println!();
    println!("Options:");
    for spec in SPECS.iter() {
        if spec.0.is_empty() {
            println!("        {:<20}{}", spec.1, spec.3);
        } else {
            println!("    {}, {:<20}{}", spec.0, spec.1, spec.3);
        }
    }
}

fn log(msg: &str) {
    if false {
        println!("{}", msg);
        std::io::stdout().flush().unwrap();
    }
}

fn add_dir(list: &Vec<String>, path: String) -> std::io::Result<()> {
    let mut new_list = list.clone();
    new_list.push(path);
    new_list.sort();
    let new_list_content = new_list.join("\n");

    write_new_lines(&new_list_content)
}

fn write_new_lines(new_list_content: &String) -> std::io::Result<()> {
    let store_path_str = get_lookup_home() + GOT_TO_FILE;
    let store_path = Path::new(store_path_str.as_str());
    log(format!("store_path: {:?}", store_path).as_str());
    if store_path.exists() {
        log("remove .got_to file");
        remove_file(store_path)?;
    }
    log("create new .got_to file");
    let mut handle = File::create(store_path)?;
    log("write new data");
    handle.write_all(new_list_content.as_bytes())?;
    handle.flush()?;

    Ok(())
}

fn add_dir_if_neccessary(finding: &Option<String>) -> Option<String> {
    log("call to add_dir_if_neccessary");
    match finding {
        Some(dir_path) => lines()
            .iter()
            .find(|&it| it == dir_path)
            .and_then(|it| Some(it.to_string()))
            .or_else(|| {
                add_dir(&lines(), dir_path.to_string()).unwrap();
                Some(dir_path.to_string())
            }),
        None => None,
    }
}

fn handle_lookup(query: Query, lines: &Vec<String>) -> std::io::Result<()> {
    let result = match query {
        Query::Single {
            ignore_case: _,
            needle,
        } if needle == "" => Some("".to_string()),
        Query::Single {
            ignore_case,
            needle,
        } => {
            log("get current working directory");
            let cwd = env::current_dir()?;
            let cwd = cwd.to_str().unwrap();
            log("find local dir");
            let finding = find_dir(&needle, cwd.to_string(), PATH_SEPARATOR);
            add_dir_if_neccessary(&finding).or_else(|| {
                find(
                    Query::Single {
                        ignore_case: ignore_case,
                        needle: needle,
                    },
                    lines,
                )
            })
        }
        _ => find(query, lines),
    };
    match result {
        Some(line) => println!("{}", line),
        None => process::exit(404),
    }

    Ok(())
}

fn print_orphaned() {
    lines()
        .iter()
        .filter(|it| !Path::new(it).is_dir())
        .for_each(|line| println!("{}", line));
}

fn remove_dead_paths() -> std::io::Result<()> {
    write_new_lines(
        &lines()
            .into_iter()
            .filter(|it| Path::new(it).is_dir())
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

fn remove_paths(paths: &Vec<String>) -> std::io::Result<()> {
    write_new_lines(
        &lines()
            .into_iter()
            .filter(|it| !paths.contains(it))
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let (action, query) = parse_args(args);
    let lines = &lines();
    match action {
        ActionType::PrintVersion => {
            println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
        }
        ActionType::PrintHelp => print_help(),
        ActionType::Lookup => handle_lookup(query, lines)?,
        ActionType::List => filter(query, lines)
            .iter()
            .for_each(|it| println!("{}", it)),
        ActionType::Clean => remove_dead_paths()?,
        ActionType::RemoveSingle => match query {
            Query::Single {
                ignore_case: _,
                needle,
            } if needle == "" => handle_error(Error::EmptyQuery),
            _ => {
                let result = find(query, lines).map(|it| [it]);
                match result {
                    Some(it) => remove_paths(&it.to_vec())?,
                    None => (),
                }
            }
        },
        ActionType::RemoveList => match query {
            Query::Single {
                ignore_case: _,
                needle,
            } if needle == "" => handle_error(Error::EmptyQuery),
            _ => remove_paths(&filter(query, lines))?,
        },
        ActionType::PrintOrphaned => print_orphaned(),
    }
    Ok(())
}
