use std::env;


use crate::goto_lookup::Query;

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
    let state = args.iter().fold(state, |acc, it| -> ParsedArgState {
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
        [] => (state.0, Query::Single(state.1, string(""))),
        [a] => (state.0, Query::Single(state.1, a.to_string())),
        _ => (state.0, Query::Multi(state.1, state.2.clone())),
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

fn main() {
    let args: Vec<String> = env::args().collect();
    let (action, _) = parse_args(args);
    match action {
        ActionType::PrintVersion => println!("goto_lookup 0.0.1"),
        ActionType::PrintHelp => print_help(),
        ActionType::Lookup => todo!(),
        ActionType::List => todo!(),
        ActionType::Clean => todo!(),
    }
}
