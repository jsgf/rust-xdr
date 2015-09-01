use super::grammar;

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
    let specs = vec!["const in = 1;",
                     "const intt = 2;",
                     "const intint = 3;",
                     "struct unsignedint { int/**/foo;\n\tint\nbar; void;\n};",
                     ];

    for sp in specs {
        let s = grammar::specification(sp);
        println!("spec sp \"{}\" => {:?}", sp, s);
        assert!(s.is_ok())
    }
}

#[test]
fn kwnames() {
    let specs = vec!["const int = 1;",
                     "struct void { int i; };",
                     "struct foo { int int; };",
                     "typedef int int;",
                     ];

    for sp in specs {
        let s = grammar::specification(sp);
        println!("spec {:?}", s);
        assert!(s.is_err())
    }
}
