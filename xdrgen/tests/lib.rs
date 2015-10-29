extern crate xdrgen;
extern crate xdr_codec;

use std::env;
use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::{Cursor, Write};
use std::process::Command;
use std::result;

use xdrgen::{generate, compile};
use xdr_codec::{Result, Error};

fn verify_gen<P: AsRef<Path>>(xdr_spec: &str,
                              prog_spec: &str,
                              outspec: P,
                              outprog: P,
                              outexe: P)
                              -> Result<()> {
    let fspec = try!(File::create(outspec));
    try!(generate("", Cursor::new(xdr_spec.as_bytes()), fspec));
    let mut fprog = try!(File::create(&outprog));
    try!(fprog.write_all(prog_spec.as_bytes()));
    
    let compile = try!(Command::new("rustc")
                       .arg("-L")
                       .arg("target/debug/deps")
                       .arg("-o")
                       .arg(outexe.as_ref())
                       .arg(outprog.as_ref())
                       .output());
    println!("stdout: {}\n, stderr: {}",
             String::from_utf8_lossy(&compile.stdout),
             String::from_utf8_lossy(&compile.stderr));
    if compile.status.success() {
        Ok(())        
    } else {
        Err(Error::Generic("couldn't compile".to_string()))
    }
}

fn verbose_remove<P: AsRef<Path>>(path: P, msg: &str) {
    if let result::Result::Err(e) = fs::remove_file(path) {
        println!("{}: {:?}", msg, e);
    }
}

// returns specpath, progpath, expath
fn get_paths(name: &str) -> (PathBuf, PathBuf, PathBuf) {
    let cdir = env::current_dir().unwrap();
    let mut specpath = cdir.clone();
    let mut progpath = cdir.clone();
    let mut exepath = cdir.clone();

    specpath.push(format!("tests/scratch/{}_xdr.rs", name));
    progpath.push(format!("tests/scratch/{}.rs", name));
    exepath.push(format!("tests/scratch/{}", name));
    (specpath, progpath, exepath)
}

#[test]
fn typedef_arrays() {
    let prog = r#"
extern crate xdr_codec;

mod typedefs {
    use xdr_codec;

    #[allow(dead_code)]
    include!("typedefs_xdr.rs");
}

fn main() {
    println!("wheew");
}
"#;
    let spec = r#"
typedef opaque buf1<20>;
typedef opaque buf2[10];
typedef opaque buf3<>;
"#;

    let (outfile, outprog, outexe) = get_paths("typedefs");
    let res = verify_gen(spec, prog, &outfile, &outprog, &outexe);
    verbose_remove(&outfile, "error removing xdr spec");
    verbose_remove(&outprog, "error removing prog");
    verbose_remove(&outexe, "error removing exe");
    assert!(res.is_ok());
}

#[test]
fn union_with_default() {
    let prog = r#"
extern crate xdr_codec;

mod foo {
    use xdr_codec;

    #[allow(dead_code)]
    include!("union_default_xdr.rs");
}

fn main() {
    println!("wheew");
}
"#;
    let xdr_spec = r#"
union foo switch (int bar) {
case 1:
    int val;
default:
    void;
};
"#;
    let (outfile, outprog, outexe) = get_paths("union_default");
    let res = verify_gen(xdr_spec, prog, &outfile, &outprog, &outexe);
    verbose_remove(&outfile, "error removing xdr spec");
    verbose_remove(&outprog, "error removing prog");
    verbose_remove(&outexe, "error removing exe");
    assert!(res.is_ok())
}

#[test]
fn union_default_nonempty() {
    let prog = r#"
extern crate xdr_codec;

mod foo {
    use xdr_codec;

    #[allow(dead_code)]
    include!("union_default_nonempty_xdr.rs");
}

fn main() {
    println!("wheew");
}
"#;
    let xdr_spec = r#"
union foo switch (int bar) {
case 1:
    int val;
default:
    opaque buf<>;
};
"#;
    let (outfile, outprog, outexe) = get_paths("union_default_nonempty");
    let res = verify_gen(xdr_spec, prog, &outfile, &outprog, &outexe);
    verbose_remove(&outfile, "error removing xdr spec");
    verbose_remove(&outprog, "error removing prog");
    verbose_remove(&outexe, "error removing exe");
    assert!(res.is_ok())
}
