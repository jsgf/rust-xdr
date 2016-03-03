# Rust XDR library

[![Build Status](https://travis-ci.org/jsgf/rust-xdr.svg?branch=master)](https://travis-ci.org/jsgf/rust-xdr)
[![Crates.io](https://img.shields.io/crates/v/xdr-codec.svg)](https://crates.io/crates/xdr-codec/)

This repo contains two crates:
  * [xdr-codec](xdr-codec), a runtime library to encode and decode XDR types
  * [xdrgen](xdrgen), a code generator which parses XDR specs (RFC4506) and
    generates Rust type definitions, with code to serialize/deserialize
    them as XDR.

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
 * MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
