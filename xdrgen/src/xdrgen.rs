#![crate_type = "bin"]

extern crate xdrgen;
extern crate env_logger;
extern crate clap;

use std::fs::File;
use std::io::{BufReader, Write};
use std::io::{stdin, stdout, stderr};

use clap::App;

use xdrgen::generate;

fn main() {
    let _ = env_logger::init();

    let matches = App::new("XDR code generator")
        .arg_from_usage("[FILE] 'Set .x file'")
        .get_matches();

    let output = stdout();
    let mut err = stderr();

    let res = if let Some(fname) = matches.value_of("FILE") {
        generate(fname, BufReader::new(File::open(fname).unwrap()), output)
    } else {
        generate("stdin", BufReader::new(stdin()), output)
    };

    if let Err(e) = res {
        let _ = writeln!(&mut err, "Failed: {}", e);
    }
}
