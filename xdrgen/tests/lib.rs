extern crate xdrgen;
extern crate xdr_codec;
extern crate tempdir;

use std::fs::{create_dir_all, File};
use std::io::{Cursor, Write};
use std::process::Command;

use xdrgen::generate;
use xdr_codec::{Result, Error};

fn build_test(name: &str, xdr_spec: &str) -> Result<()> {
    let tempdir = tempdir::TempDir::new("build").expect("Failed to make tempdir");
    let dir = tempdir.path();

    println!("tempdir {:?}", dir);
    let _ = create_dir_all(&dir);

    let mainfile = dir.join(format!("{}.rs", name));
    let testfile = dir.join(format!("{}_xdr.rs", name));
    let exefile = dir.join(format!("{}", name));

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

    let compile = {
        let mut cmd = Command::new("rustc");
        let cmd = cmd
               .current_dir(std::env::current_dir()?)
               .arg("--crate-type").arg("bin")
               .arg("--crate-name").arg(name)
		       .arg("-L").arg("../target/debug/deps")
		       .arg("--extern").arg("xdr_codec=../target/debug/libxdr_codec.rlib")
		       .arg("-o").arg(exefile)
		       //.arg("-Z").arg("no-trans")
		       .arg(mainfile);
        println!("CWD: {:?} Command: {:?}", std::env::current_dir(), cmd);
        cmd.output()?
    };

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
fn recursive_type() {
    let name = "recursive_type";
    let spec = r#"
struct list { list *next; };
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

#[test]
fn simple() {
    let name = "simple";
    let specs = vec!["struct foo { int bar; unsigned int blat; hyper foo; unsigned hyper \
                      hyperfoo; };",
                     "const blop = 123;",
                     "typedef opaque Ioaddr<>;"];

    for (i, spec) in specs.into_iter().enumerate() {
        let name = format!("{}_{}", name, i);

        if let Err(e) = build_test(&name, spec) {
            panic!("test {} failed: {}", name, e);
        }
    }
}

#[test]
fn rfc4506() {
    let name = "rfc4506";
    let spec = r#"

         const MAXUSERNAME = 32;     /* max length of a user name */
         const MAXFILELEN = 65535;   /* max length of a file      */
         const MAXNAMELEN = 255;     /* max length of a file name */

         /*
          * Types of files:
          */
         enum filekind {
            TEXT = 0,       /* ascii data */
            DATA = 1,       /* raw data   */
            EXEC = 2        /* executable */
         };

         /*
          * File information, per kind of file:
          */
         union filetype switch (filekind kind) {
         case TEXT:
            void;                           /* no extra information */
         case DATA:
            string creator<MAXNAMELEN>;     /* data creator         */
         case EXEC:
            string interpretor<MAXNAMELEN>; /* program interpretor  */
         };

         /*
          * A complete file:
          */
         struct file {
            string filename<MAXNAMELEN>; /* name of file    */
            filetype type_;              /* info about file */
            string owner<MAXUSERNAME>;   /* owner of file   */
            opaque data<MAXFILELEN>;     /* file data       */
         };
"#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn enums() {
    let name = "enums";
    let spec = r#"
        enum Foo {
            A = 0,
            B = -1
        };
        struct Bar { Foo x; };
    "#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn unions() {
    let name = "unions";
    let spec = r#"
        enum Foo {
            A = 0,
            B = -1
        };
        union foo switch (Foo bar) {
        case A: int val;
        case B: void;
        default: int other;
        };
        union foo2 switch (Foo bar) {
        case A: void;
        case B: int a;
        default: int other;
        };
    "#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn consts() {
    let name = "consts";
    let spec = r#"
        const FOO = 1;
        const BAR = -1;
    "#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn arrays() {
    let name = "arrays";
    let spec = r#"
        struct a { opaque data[15]; };
        struct b { int things[10]; };
        struct c { string decitweet[14]; };
        struct d { c tweetses[10]; };
    "#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}

#[test]
fn flex() {
    let name = "flex";
    let spec = r#"
        struct a { opaque data<>; opaque limdata<15>; };
        struct b { string s<>; string limstr<32>; };
        struct c { a athing<>; a alim<10>; };
    "#;

    if let Err(e) = build_test(name, spec) {
        panic!("test {} failed: {}", name, e);
    }
}
