extern crate xdr;

use xdr::compile;

fn main() {
    compile("src/simple.x").unwrap();
}
