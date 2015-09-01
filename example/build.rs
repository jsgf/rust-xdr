extern crate xdrgen;

fn main() {
    xdrgen::compile("src/simple.x").unwrap();
}
