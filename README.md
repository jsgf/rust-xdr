Rust XDR library
================

This repo contains two crates:
  * (xdr-codec), a runtime library to encode and decode XDR types
  * (xdrgen), a code generator which parses XDR specs (RFC4506) and
    generates Rust type definitions, with code to serialize/deserialize
    them as XDR.
