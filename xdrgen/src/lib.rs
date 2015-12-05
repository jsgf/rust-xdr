//! XDR codec generation
//!
//! This crate provides library interfaces for programatically generating Rust code to implement
//! RFC4506 XDR encoding/decoding, as well as a command line tool "xdrgen".
//!
//! It is intended to be used with the "xdr-codec" crate, which provides the runtime library for
//! encoding/decoding primitive types, strings, opaque data and arrays.
#![feature(slice_patterns, plugin, rustc_private, quote, box_patterns)]
#![plugin(peg_syntax_ext)]
#![crate_type = "lib"]
extern crate syntax;
extern crate xdr_codec as xdr;

#[macro_use]
extern crate log;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::{Read, Write};
use std::error::Error;
use std::fmt::Display;
use std::env;
use std::iter::FromIterator;
use std::fmt::Debug;
use std::result;

use xdr::Result;

mod spec;
use spec::{Symtab, Emit, Emitpack};
use spec::{with_fake_extctxt, rustast, specification};

// Given an iterator returning results, return a result containing
// either the first error or a an Ok collection.
fn fold_result<I, T, E, C>(it: I) -> result::Result<C, E>
    where I: IntoIterator<Item=result::Result<T, E>>, C: FromIterator<T>, E: Debug
{
    let (good, bad): (_, Vec<_>) = it.into_iter().partition(|res| res.is_ok());

    let badness = bad.into_iter().fold(None, |cur, res|
                                       match cur {
                                           None => Some(res),
                                           Some(v) => Some(v),
                                       });

    match badness {
        Some(Err(b)) => Err(b),
        Some(Ok(_)) => panic!("Ok on the bad list"),
        None => Ok(good.into_iter().map(|r| r.unwrap()).collect()),
    }
}

/*
fn option_result<T, E>(optres: Option<result::Result<T, E>>) -> result::Result<Option<T>, E> {
    match optres {
        None => Ok(None),
        Some(Err(e)) => Err(e),
        Some(Ok(v)) => Ok(Some(v)),
    }
}
 */

fn result_option<T, E>(resopt: result::Result<Option<T>, E>) -> Option<result::Result<T, E>> {
    match resopt {
        Ok(None) => None,
        Ok(Some(v)) => Some(Ok(v)),
        Err(e) => Some(Err(e)),
    }
}

#[test]
fn test_fold_result() {
    let good = vec![Ok(1), Ok(2), Ok(3)];
    assert_eq!(fold_result::<_,_,&str,_>(good), Ok(vec![1,2,3]));

    let bad = vec![Ok(1), Ok(2), Err("bad")];
    assert_eq!(fold_result::<_,_,_,Vec<u32>>(bad), Err("bad"));

    let worse = vec![Ok(1), Err("worse"), Err("bad")];
    assert_eq!(fold_result::<_,_,_,Vec<u32>>(worse), Err("worse"));
}

/// Generate Rust code from an RFC4506 XDR specification
///
/// `infile` is simply a string used in error messages; it may be empty. `input` is a read stream of
/// the specification, and `output` is where the generated code is sent.
pub fn generate<In, Out>(infile: &str, mut input: In, mut output: Out) -> Result<()>
    where In: Read, Out: Write
{
    let mut source = String::new();

    try!(input.read_to_string(&mut source));

    let xdr = match spec::specification(&source) {
        Ok(defns) => Symtab::new(&defns),
        Err(e) => return Err(xdr::Error::from(format!("parse error: {}", e))),
    };

    let res: Result<Vec<_>> = with_fake_extctxt(|e| {
        let consts = xdr.constants()
            .filter_map(|(c, &(v, ref scope))| {
                if scope.is_none() {
                    Some(spec::Const(c.clone(), v))
                } else {
                    None
                }
            })
            .map(|c| c.define(&xdr, e));

        let typespecs = xdr.typespecs()
            .map(|(n, ty)| spec::Typespec(n.clone(), ty.clone()))
            .map(|c| c.define(&xdr, e));

        let typesyns = xdr.typesyns()
            .map(|(n, ty)| spec::Typesyn(n.clone(), ty.clone()))
            .map(|c| c.define(&xdr, e));

        let packers = xdr.typespecs()
            .map(|(n, ty)| spec::Typespec(n.clone(), ty.clone()))
            .filter_map(|c| result_option(c.pack(&xdr, e)));

        let unpackers = xdr.typespecs()
            .map(|(n, ty)| spec::Typespec(n.clone(), ty.clone()))
            .filter_map(|c| result_option(c.unpack(&xdr, e)));

        let module: Vec<_> = try!(fold_result(consts.chain(typespecs).chain(typesyns).chain(packers).chain(unpackers)));

        Ok(module.iter().map(|it| rustast::item_to_string(it)).collect())
    });

    let res = try!(res);

    let _ = writeln!(output, r#"
// GENERATED CODE
//
// Generated from {}.
//
// DO NOT EDIT

"#, infile);

    for it in res {
        let _ = writeln!(output, "{}\n", it);
    }

    Ok(())
}

/// Simplest possible way to generate Rust code from an XDR specification.
///
/// It is intended for use in a build.rs script:
///
/// ```ignore
/// extern crate xdrgen;
///
/// fn main() {
///    xdrgen::compile("src/simple.x").unwrap();
/// }
/// ```
///
/// Output is put into OUT_DIR, and can be included:
///
/// ```ignore
/// mod simple {
///    use xdr_codec;
///
///    include!(concat!(env!("OUT_DIR"), "/simple_xdr.rs"));
/// }
/// ```
///
/// If your specification uses types which are not within the specification, you can provide your
/// own implementations of `Pack` and `Unpack` for them.
pub fn compile<P>(infile: P) -> Result<()>
    where P: AsRef<Path> + Display
{
    let input = try!(File::open(&infile));

    let mut outdir = PathBuf::from(env::var("OUT_DIR").unwrap_or(String::from(".")));
    let outfile = PathBuf::from(infile.as_ref()).file_stem().unwrap().to_owned().into_string().unwrap().replace("-", "_");

    outdir.push(&format!("{}_xdr.rs", outfile));

    let output = try!(File::create(outdir));

    generate(infile.as_ref().as_os_str().to_str().unwrap_or("<unknown>"), input, output)
}
