Rust XDR library
================

[![Build Status](https://travis-ci.org/jsgf/rust-xdr.svg?branch=master)](https://travis-ci.org/jsgf/rust-xdr)

This repo contains two crates:
  * [xdr-codec](xdr-codec), a runtime library to encode and decode XDR types
  * [xdrgen](xdrgen), a code generator which parses XDR specs (RFC4506) and
    generates Rust type definitions, with code to serialize/deserialize
    them as XDR.
