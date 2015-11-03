extern crate xdrgen;
extern crate xdr_codec;

use std::env;
use std::fs::{create_dir_all, File};
use std::path::PathBuf;
use std::io::{Cursor, Write};
use std::process::Command;

use xdrgen::{generate, compile};
use xdr_codec::{Result, Error};

fn build_test(name: &str, xdr_spec: &str) -> Result<()> {
    let dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    let _ = create_dir_all(&dir);
    
    let mainfile = dir.with_file_name(format!("{}.rs", name));
    let testfile = dir.with_file_name(format!("{}_xdr.rs", name));
    let exefile = dir.with_file_name(format!("{}", name));

    let template = format!(r#"
extern crate xdr_codec;

#[allow(dead_code, non_camel_case_types)]
mod test {{
    use xdr_codec;
    include!("{}");
}}

fn main() {{}}
"#,
                           testfile.as_os_str().to_string_lossy());

    {
        let mut main = try!(File::create(&mainfile));
        try!(main.write_all(template.as_bytes()));
    }

    {
        let test = try!(File::create(&testfile));
        try!(generate(name, Cursor::new(xdr_spec.as_bytes()), test));
    }

    let compile = try!(Command::new("rustc")
                       .arg("-L").arg(dir.with_file_name("../../deps"))
                       .arg("-o").arg(exefile)
                       .arg("-Z").arg("no-trans")
                       .arg(mainfile)
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

#[test]
fn typedef_arrays() {
    let name = "typedef_arrays";
    let spec = r#"
typedef opaque buf1<20>;
typedef opaque buf2[10];
typedef opaque buf3<>;
"#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn union_with_default() {
    let name = "union_with_default";
    let spec = r#"
union foo switch (int bar) {
case 1:
    int val;
default:
    void;
};
"#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn union_default_nonempty() {
    let name = "union_default_nonempty";
    let spec = r#"
union foo switch (int bar) {
case 1:
    int val;
default:
    opaque buf<>;
};
"#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}
