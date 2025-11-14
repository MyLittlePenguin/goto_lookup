use std::env;
use std::fs;


// type SingleQuery = (bool, String)
// type MultiQuery = (bool, Vec<String>)

pub enum Query {
    Single(bool, String),
    Multi(bool, Vec<String>),
}

static GOT_TO_FILE: &str = "/.got_to";

pub fn home() -> &str {
    match env::home_dir() {
        Some(path) => path.to_str().unwrap(),
        None => panic!("home directory not found!"),
    }
}

pub fn lines() -> Vec<String> {
    return match fs::read_to_string(home() + GOT_TO_FILE) {
        Ok(content) => Vec::from_iter(content.split("\n").into_iter().map(|it| it.to_string())),
        Err(error) => vec![],
    };
}

pub fn filter(query: Query) -> Vec<String> {
    vec![]
}
