#![feature(slice_patterns, plugin, quote, box_patterns)]
#![crate_type = "bin"]

extern crate xdr;

use std::env;
use std::fs::File;
use std::io::BufReader;
use std::io::{stdin, stdout};

use xdr::generate;

fn print_usage(prog: &str) {
    println!("Usage: {} [file]", prog)
}

fn main() {
    let args: Vec<_> = env::args_os().collect();
    let progname = args[0].to_str().unwrap();
    let output = stdout();
    
    match &args[1..] {
	[ref arg] if arg.to_str() == Some("-h") => return print_usage(progname),
	[ref fname] => generate(fname.as_os_str().to_str().unwrap_or("<unknown>"),
                                BufReader::new(File::open(fname).unwrap()), output).unwrap(),
	[] => generate("stdin", BufReader::new(stdin()), output).unwrap(),
	_ => return print_usage(progname),
    };
}
