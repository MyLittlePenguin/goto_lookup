use std::{env, fs::File, fs::remove_file, io::Write, path::Path};

use crate::goto_lookup::{GOT_TO_FILE, Query, filter, find, find_dir, home};
use std::process;

pub mod goto_lookup;

enum ActionType {
    PrintVersion,
    PrintHelp,
    Lookup,
    List,
    Clean,
}

fn string(s: &str) -> String {
    s.to_string()
}

type ParsedArgState<'a> = (ActionType, bool, &'a mut Vec<String>);
type ArgSpec<'a, 'b, 'c> = (
    &'a str,
    &'b str,
    fn(ParsedArgState) -> ParsedArgState,
    &'c str,
);

const SPECS: [ArgSpec; 6] = [
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
        |state| (ActionType::List, state.1, state.2),
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
    println!("Usage: goto_lookup [options] [query]");
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

    let store_path_str = home() + GOT_TO_FILE;
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
        Some(dir_path) => goto_lookup::lines()
            .iter()
            .find(|&it| it == dir_path)
            .and_then(|it| Some(it.to_string()))
            .or_else(|| {
                add_dir(&goto_lookup::lines(), dir_path.to_string()).unwrap();
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
            let finding = find_dir(&needle, cwd.to_string(), "/");
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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let (action, query) = parse_args(args);
    let lines = &goto_lookup::lines();
    match action {
        ActionType::PrintVersion => println!("goto_lookup 0.0.2"),
        ActionType::PrintHelp => print_help(),
        ActionType::Lookup => handle_lookup(query, lines)?,
        ActionType::List => filter(query, lines)
            .iter()
            .for_each(|it| println!("{}", it)),
        ActionType::Clean => todo!(),
    }
    Ok(())
}
