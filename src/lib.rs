#![feature(slice_patterns, plugin, rustc_quote, box_patterns)]
#![plugin(peg_syntax_ext)]
#![crate_type = "lib"]

extern crate syntax;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::fmt::Display;
use std::env;

mod xdr;

mod spec;
use spec::{Symtab, Emit, Emitpack};
use spec::{with_fake_extctxt, rustast, specification};

pub fn generate<In, Out>(infile: &str, mut input: In, mut output: Out) -> Result<()>
    where In: Read, Out: Write
{
    let mut source = String::new();

    try!(input.read_to_string(&mut source));

    let xdr = match spec::specification(&source) {
        Ok(defns) => Symtab::new(&defns),
        Err(err) => return Err(Error::from(err)),
    };
    
    with_fake_extctxt(|e| {
        let consts = xdr.constants()
            .filter_map(|(c, &(v, ref scope))| {
                if scope.is_none() {
                    Some(spec::Const(c.clone(), v))
                } else {
                    None
                }
            })
            .map(|c| c.define(&xdr, e));

        let typedefs = xdr.typedefs()
            .map(|(n, ty)| spec::Typedef(n.clone(), ty.clone()))
            .map(|c| c.define(&xdr, e));

        let packers = xdr.typedefs()
            .map(|(n, ty)| spec::Typedef(n.clone(), ty.clone()))
            .filter_map(|c| c.pack(&xdr, e));
        
        let unpackers = xdr.typedefs()
            .map(|(n, ty)| spec::Typedef(n.clone(), ty.clone()))
            .filter_map(|c| c.unpack(&xdr, e));

        let module = consts.chain(typedefs).chain(packers).chain(unpackers);

        let _ = writeln!(output, r#"
// GENERATED CODE
//
// Generated from {}.
//
// DO NOT EDIT

"#, infile);
        for it in module {
            let _ = writeln!(output, "{}\n", rustast::item_to_string(&*it));
        }
    });

    Ok(())
}

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

