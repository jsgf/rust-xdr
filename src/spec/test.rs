use super::grammar;
use super::super::generate;
use std::io::Cursor;

#[test]
fn simple() {
    let specs = vec!["struct foo { int bar; unsigned int blat; hyper foo; unsigned hyper hyperfoo; };",
                     "const blop = 123;",
                     "typedef opaque Ioaddr<>;"
                     ];

    for sp in specs {
        let s = grammar::specification(sp);
        println!("spec sp \"{}\" => {:?}", sp, s);
        assert!(s.is_ok())
    }
}

#[test]
fn rfc4506() {
    let s = grammar::specification(r#"

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
            filetype type;               /* info about file */
            string owner<MAXUSERNAME>;   /* owner of file   */
            opaque data<MAXFILELEN>;     /* file data       */
         };
"#);
    println!("spec {:?}", s);
    assert!(s.is_ok())
}
    

#[test]
fn typedef_void() {
    let s = grammar::specification(r#"
typedef void;           /* syntactically defined, semantically meaningless  */
"#);

        println!("spec {:?}", s);
        assert!(s.is_err())
}

#[test]
fn kwishnames() {
    let kws = vec!["bool", "case", "const", "default", "double", "enum", "float",
                   "hyper", "int", "opaque", "quadruple", "string", "struct",
                   "switch", "typedef", "union", "unsigned", "void"];
    let specs = vec!["const {}x = 1;",
                     "struct {}x { int i; };",
                     "struct foo { int {}x; };",
                     "typedef int {}x;",
                     "union {}x switch (int x) { default: void; };",
                     "union x switch (int {}x) { default: void; };",
                     "union x switch (int y) { case 1: int {}x; };",
                     ];

    for sp in &specs {
        for kw in &kws {
            let spec = sp.replace("{}", kw);
            let s = grammar::specification(&spec);
            println!("spec {} => {:?}", spec, s);
            assert!(s.is_ok())
        }
    }
}

#[test]
fn kwnames() {
    let kws = vec!["bool", "case", "const", "default", "double", "enum", "float",
                   "hyper", "int", "opaque", "quadruple", "string", "struct",
                   "switch", "typedef", "union", "unsigned", "void"];
    let specs = vec!["const {} = 1;",
                     "struct {} { int i; };",
                     "struct foo { int {}; };",
                     "typedef int {};",
                     "union {} switch (int x) { default: void; };",
                     "union x switch (int {}) { default: void; };",
                     "union x switch (int y) { case 1: int {}; };",
                     ];

    for sp in &specs {
        for kw in &kws {
            let spec = sp.replace("{}", kw);
            let s = grammar::specification(&spec);
            println!("spec {} => {:?}", spec, s);
            assert!(s.is_err())
        }
    }
}

#[test]
fn inline_struct() {
    let spec = r#"
        struct thing {
                struct { int a; int b; } thing;
        };
"#;
    let s = grammar::specification(spec);

    println!("spec {:?}", s);
    assert!(s.is_ok());

    let g = generate("", Cursor::new(spec.as_bytes()), Vec::new());
    assert!(g.is_err());
}

#[test]
fn inline_union() {
    let spec = r#"
        struct thing {
                union switch(int x) { case 0: int a; case 1: int b; } thing;
        };
"#;
    let s = grammar::specification(spec);

    println!("spec {:?}", s);
    assert!(s.is_ok());

    let g = generate("", Cursor::new(spec.as_bytes()), Vec::new());
    assert!(g.is_err());
}
