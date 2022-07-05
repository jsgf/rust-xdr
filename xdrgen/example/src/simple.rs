extern crate xdr_codec;

use std::io::Cursor;
use xdr_codec::{pack, unpack};

mod simple {
    use xdr_codec;

    #[allow(dead_code)]
    include!(concat!(env!("OUT_DIR"), "/simple_xdr.rs"));
}

fn main() {
    let foo = simple::Foo {
        a: 1,
        b: 2,
        c: 3,
        bar: vec![simple::Bar {
            data: vec![1, 2, 3],
        }],
        barish: None,
        name: String::from("foox"),
        thing: simple::Things::C,
        type_: 123,
    };

    let mut buf = Vec::new();

    pack(&foo, &mut buf).unwrap();
    println!("foo={:?}", foo);
    println!("buf={:?} len={}", buf, buf.len());

    let mut cur = Cursor::new(buf);

    let foo2 = unpack(&mut cur).unwrap();

    println!("foo={:?}", foo);
    println!("foo2={:?}", foo2);
    assert_eq!(foo, foo2);
}
