use std::env;

enum ActionType {
    PrintVersion,
    PrintHelp,
    Lookup,
    List,
    Clean,
}

enum Query {
    Single(bool, String),
    Multi(bool, Vec<String>),
}

fn string(s: &str) -> String {
    s.to_string()
}

fn parse_args(args: Vec<String>) -> (ActionType, Query) {
    let state: (ActionType, bool, &mut Vec<String>) = (ActionType::Lookup, false, &mut vec![]);
    let state = args.iter().fold(state, |acc, it| match it.as_str() {
        "-i" | "--ignore-case" => (acc.0, true, acc.2),
        "-I" => (acc.0, false, acc.2),
        "-l" | "--list" => (ActionType::List, acc.1, acc.2),
        "-v" | "--version" => (ActionType::PrintVersion, acc.1, acc.2),
        "-h" | "--help" => (ActionType::PrintHelp, acc.1, acc.2),
        "--clean" => (ActionType::Clean, acc.1, acc.2),
        str => {
            acc.2.push(str.to_string());
            acc
        }
    });
    match &state.2[..] {
        [] => (state.0, Query::Single(state.1, string(""))),
        [a] => (state.0, Query::Single(state.1, a.to_string())),
        _ => (state.0, Query::Multi(state.1, state.2.clone())),
    }
}

fn main() {
    println!("Hello, world!");
    let args: Vec<String> = env::args().collect();
    let (action, query) = parse_args(args);
    match action {
        ActionType::PrintVersion => println!("goto_lookup 0.0.1"),
        ActionType::PrintHelp => todo!(),
        ActionType::Lookup => todo!(),
        ActionType::List => todo!(),
        ActionType::Clean => todo!(),
    }
}
