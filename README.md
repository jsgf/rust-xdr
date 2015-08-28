Rust XDR library
================

This crate provides:
  * xdrgen, which takes an XDR specification in a .x file, and produces Rust
    code to serialize and deserialize the specified types, and
  * A set of runtime code, which is used by the generated code, but can also
    be used to hand-code codecs.

The syntax of the .x file follows RFC4506. This has type definitions for
XDR but does not include RPC protocol specifications. Correspondingly, xdrgen
does not support auto-generation of RPC clients/servers.

Usage
-----
Usage is straightforward. You can generate the Rust code from a spec with:
`xdrgen spec.x > spec_xdr.rs`. This code can then be included into a module:
```
mod myspec {
  use xdr::xdr;

  include!("spec_xdr.rs");
}
```

Once you have this, you can call `xdr::pack(&mytype, &mut output)`, and
`let mything: MyThing = xdr::unpack(&mut input).unwrap()`.

The serializers require your types to implement the `Pack` and `Unpack`
traits, and generate code to write to `std::io::Write` implementation, and
read from `std::io::Read`.

All types and fields are generated public, so you can control their access
outside your module or crate. If your spec references other types which are
not defined within the spec, then you can define them within the module
as well, either by aliasing them with other defined types, or implementing
the `Pack` and `Unpack` traits yourself.

TODO: build script helpers
Limitations
-----------
There are currently a few limitations:
   * The generated code uses identifiers as specified in the .x file, so the
     Rust code will not use normal formatting conventions.
   * It also does not filter for rust keywords, so XDR specifications intended
     for C may use identifiers like `type`.
   * XDR allows for anonymous structures defined as part of a structure field
     declaration, Rust syntax does not allow for this. `xdrgen` does not yet
     autogenerate names for these structures.
   * XDR has discriminated unions, which are a good match for Rust enums.
     However, it also supports a `default` case if an unknown discriminator
     is encountered. This crate supports this for unpacking, but not for
     packing, as Rust does not allow enums to have unknown values.
   * Code generated for unpacking fixed-sized arrays is always fully unwound,
     since it represents these as Rust fixed-sized arrays, and there's currently
     no way to initialize them safely except by enumerating all the elements.
