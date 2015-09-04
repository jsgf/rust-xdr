#![feature(slice_patterns, plugin, quote, box_patterns)]
#![crate_type = "bin"]

extern crate xdrgen;

use std::env;
use std::fs::File;
use std::io::{BufReader, Write};
use std::io::{stdin, stdout, stderr};

use xdrgen::generate;

fn print_usage(prog: &str) {
    let mut err = stderr();
    let _ = writeln!(&mut err, "Usage: {} [file]", prog);
}

fn main() {
    let args: Vec<_> = env::args_os().collect();
    let progname = args[0].to_str().unwrap();
    let output = stdout();
    let mut err = stderr();

    match &args[1..] {
	[ref arg] if arg.to_str() == Some("-h") => return print_usage(progname),
	[ref fname] => {
            match generate(fname.as_os_str().to_str().unwrap_or("<unknown>"),
                           BufReader::new(File::open(fname).unwrap()), output) {
                Ok(()) => (),
                Err(e) => { let _ = writeln!(&mut err, "Failed: {}", e); },
            }
        },
	[] => {
            match generate("stdin", BufReader::new(stdin()), output) {
                Ok(()) => (),
                Err(e) => { let _ = writeln!(&mut err, "Failed: {}", e); },
            }
        },
	_ => return print_usage(progname),
    };
}
