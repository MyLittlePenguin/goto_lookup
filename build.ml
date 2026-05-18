#!/usr/bin/env utop

let rec run = function
  | "run" -> cmd "cargo run";
  | "help" -> cmd "cargo run -- --help";
  | "" | "build" | "compile" ->
    cmd "cargo build";
  | "release" -> cmd "cargo build --release";
  | "windows" -> cmd "cargo build --release --target=x86_64-pc-windows-gnu"
  | "install" -> cmd "cargo install --path .";
  | "test" -> cmd "cargo test";
  | "pull" -> cmd "git pull";
  | _ -> exit 404;;
