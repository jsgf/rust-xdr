extern crate xdrgen;

fn main() {
    println!("cargo:rerun-if-changed=src/simple.x");
    xdrgen::compile("src/simple.x").unwrap();
}
