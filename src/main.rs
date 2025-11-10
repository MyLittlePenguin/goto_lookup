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

type ParsedArgState<'a> = (ActionType, bool, &'a mut Vec<String>);
type ArgSpec<'a, 'b> = (&'a str, &'b str, fn(ParsedArgState) -> ParsedArgState);

const SPECS: [ArgSpec; 6] = [
    ("-i", "--ignore-case", |state| {
        (state.0, true, state.2)
    }),
    ("-I", "", |state| (state.0, false, state.2)),
    ("-l", "--list", |state| {
        (ActionType::List, state.1, state.2)
    }),
    ("-v", "--version", |state| {
        (ActionType::PrintVersion, state.1, state.2)
    }),
    ("-h", "--help", |state| {
        (ActionType::PrintHelp, state.1, state.2)
    }),
    ("--clean", "", |state| {
        (ActionType::Clean, state.1, state.2)
    }),
];

fn parse_args(args: Vec<String>) -> (ActionType, Query) {
    let state: ParsedArgState = (ActionType::Lookup, false, &mut vec![]);
    let state = args.iter().fold(state, |acc, it| -> ParsedArgState {
        match SPECS.iter().find(|spec| {
            (!spec.0.is_empty() && spec.0 == it)
                || (!spec.1.is_empty() && spec.1 == it)
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

fn main() {
    println!("Hello, world!");
    let args: Vec<String> = env::args().collect();
    let (action, _) = parse_args(args);
    match action {
        ActionType::PrintVersion => println!("goto_lookup 0.0.1"),
        ActionType::PrintHelp => todo!(),
        ActionType::Lookup => todo!(),
        ActionType::List => todo!(),
        ActionType::Clean => todo!(),
    }
}
