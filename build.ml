#!/usr/bin/env utop

let rec run = function
  | "run" -> cmd "cargo run";
  | "help" -> cmd "cargo run -- --help";
  | "" | "build" | "compile" ->
    cmd "cargo build";
  | "release" -> cmd "cargo build --release";
  | "install" -> cmd "cargo install";
  | "test" -> cmd "cargo test";
  | "pull" -> cmd "git pull";
  | _ -> exit 404;;
