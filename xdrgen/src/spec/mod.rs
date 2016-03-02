#![allow(unused_imports, unused_mut, dead_code)]

use std::collections::btree_map::{BTreeMap, Iter};
use std::io::{stderr, Write};

use std::fmt::Debug;
use std::iter::{FromIterator, IntoIterator};
use std::result;

pub mod rustast;
mod fake_extctxt;
mod xdr_nom;

pub use self::fake_extctxt::with_fake_extctxt;

use xdr::Error;

use super::{fold_result, result_option};

pub type Result<T> = result::Result<T, Error>;

pub use self::xdr_nom::specification;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    Ident(String),
    Const(i64),
}

impl Value {
    fn as_ident(&self) -> rustast::Ident {
        match self {
            &Value::Ident(ref id) => rustast::str_to_ident(id),
            &Value::Const(val) => rustast::str_to_ident(&format!("Const{}{}",
                                                                 (if val < 0 { "_" } else { "" }),
                                                                 val.abs())),
        }
    }

    fn as_i64(&self, symtab: &Symtab) -> Option<i64> { symtab.value(self) }

    fn as_token(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Vec<rustast::TokenTree> {
        match self {
            &Value::Const(c) => quote_tokens!(ctxt, $c),
            &Value::Ident(ref id) => {
                let tok = rustast::str_to_ident(id);
                if let Some((_, Some(ref scope))) = symtab.getconst(id) {
                    let scope = rustast::str_to_ident(scope);
                    quote_tokens!(ctxt, $scope :: $tok)
                } else {
                    quote_tokens!(ctxt, $tok)
                }
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    UInt,
    Int,
    UHyper,
    Hyper,
    Float,
    Double,
    Quadruple,
    Bool,

    // Special array elements
    Opaque,                     // binary
    String,                     // text

    // Compound types
    Enum(Vec<EnumDefn>),
    Struct(Vec<Decl>),
    Union(Box<Decl>, Vec<UnionCase>, Option<Box<Decl>>),

    Option(Box<Type>),
    Array(Box<Type>, Value),
    Flex(Box<Type>, Option<Value>),

    // Type reference (may be external)
    Ident(String),
}

impl Type {
    fn array(ty: Type, sz: Value) -> Type {
        Type::Array(Box::new(ty), sz)
    }

    fn flex(ty: Type, sz: Option<Value>) -> Type {
        Type::Flex(Box::new(ty), sz)
    }

    fn option(ty: Type) -> Type {
        Type::Option(Box::new(ty))
    }

    fn union((d, c, dfl): (Decl, Vec<UnionCase>, Option<Decl>)) -> Type {
        Type::Union(Box::new(d), c, dfl.map(Box::new))
    }

    fn is_boxed(&self, symtab: &Symtab) -> bool {
        use self::Type::*;

        match self {
            _ if self.is_prim(symtab) => false,
            &Array(_, _) | &Flex(_, _) | &Option(_) => false,
            &Ident(ref name) => if let Some(ty) = symtab.typespec(name) { ty.is_boxed(symtab) } else { true },
            _ => true,
        }
    }

    fn is_prim(&self, symtab: &Symtab) -> bool {
        use self::Type::*;

        match self {
            &Int | &UInt |
            &Hyper | &UHyper |
            &Float | &Double | &Quadruple |
            &Bool       => true,

            &Ident(ref id) =>
                match symtab.typespec(id) {
                    None => false,
                    Some(ref ty) => ty.is_prim(symtab),
                },

            _           => false,
        }
    }

    fn packer(&self, val: Vec<rustast::TokenTree>, symtab: &Symtab, ctxt: &rustast::ExtCtxt)
              -> Result<Vec<rustast::TokenTree>> {
        use self::Type::*;
        use self::Decl::*;

        let res = match self {
            &Enum(_) => { quote_tokens!(ctxt, try!((*$val as i32).pack(out))) },

            &Flex(box Opaque, ref maxsz) => {
                let maxsz = match maxsz {
                    &None => quote_tokens!(ctxt, None),
                    &Some(ref mx) => {
                        let mx = mx.as_token(symtab, ctxt);
                        quote_tokens!(ctxt, Some($mx as usize))
                    },
                };
                quote_tokens!(ctxt, try!(xdr_codec::pack_opaque_flex(&$val, $maxsz, out)))
            },

            &Flex(box String, ref maxsz) => {
                let maxsz = match maxsz {
                    &None => quote_tokens!(ctxt, None),
                    &Some(ref mx) => {
                        let mx = mx.as_token(symtab, ctxt);
                        quote_tokens!(ctxt, Some($mx as usize))
                    },
                };
                quote_tokens!(ctxt, try!(xdr_codec::pack_string(&$val, $maxsz, out)))
            },

            &Flex(_, ref maxsz) => {
                let maxsz = match maxsz {
                    &None => quote_tokens!(ctxt, None),
                    &Some(ref mx) => {
                        let mx = mx.as_token(symtab, ctxt);
                        quote_tokens!(ctxt, Some($mx as usize))
                    },
                };
                quote_tokens!(ctxt, try!(xdr_codec::pack_flex($val, $maxsz, out)))
            },

            &Array(_, _) => quote_tokens!(ctxt, try!(xdr_codec::pack_array(&$val[..], out))),

            _ => quote_tokens!(ctxt, try!($val.pack(out))),
        };

        trace!("packed {:?} val {:?} => {:?}", self, val, res);
        Ok(res)
    }

    fn is_syn(&self) -> bool {
        use self::Type::*;

        match self {
            &Opaque | &String | &Array(_, _) | &Flex(_, _) | &Option(_) |
            &Ident(_) | &Int | &UInt | &Hyper | &UHyper | &Float | &Double |
            &Quadruple | &Bool
                => true,
            _ => false,
        }
    }

    fn unpacker(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Vec<rustast::TokenTree> {
        use self::Type::*;

        match self {
            &Array(box Opaque, ref value) => {
                let elems = value.as_i64(symtab).unwrap();
                let mut unpacks = Vec::new();

                // I think this is the only way to safely initialize a fixed-sized array in
                // Rust. The alternative would be to use a Vec<>, but this would need to deal with a
                // wrong-sized value, and an extra indirection.
                for _ in 0..elems {
                    unpacks.push(quote_tokens!(ctxt, { let v = try!(input.read_u8()); asz += 1; v },));
                }

                quote_tokens!(ctxt, { let mut asz = 0; let v = [ $unpacks ]; (v, asz) })
            },

            &Array(_, ref value) => {
                let elems = value.as_i64(symtab).unwrap();
                let mut unpacks = Vec::new();

                // I think this is the only way to safely initialize a fixed-sized array in
                // Rust. The alternative would be to use a Vec<>, but this would need to deal with a
                // wrong-sized value, and an extra indirection.
                for _ in 0..elems {
                    unpacks.push(quote_tokens!(ctxt, { let (v, esz) = try!(xdr_codec::Unpack::unpack(input)); asz += esz; v },));
                }

                quote_tokens!(ctxt, { let mut asz = 0; let v = [ $unpacks ]; (v, asz) })
            },

            &Flex(box String, ref maxsz) => {
                let maxsz = match maxsz {
                    &None => quote_tokens!(ctxt, None),
                    &Some(ref mx) => { let mx = mx.as_token(symtab, ctxt); quote_tokens!(ctxt, Some($mx as usize)) },
                };
                quote_tokens!(ctxt, try!(xdr_codec::unpack_string(input, $maxsz)))
            },

            &Flex(box Opaque, ref maxsz) => {
                let maxsz = match maxsz {
                    &None => quote_tokens!(ctxt, None),
                    &Some(ref mx) => { let mx = mx.as_token(symtab, ctxt); quote_tokens!(ctxt, Some($mx as usize)) },
                };
                quote_tokens!(ctxt, try!(xdr_codec::unpack_opaque_flex(input, $maxsz)))
            },

            &Flex(_, ref maxsz) => {
                let maxsz = match maxsz {
                    &None => quote_tokens!(ctxt, None),
                    &Some(ref mx) => { let mx = mx.as_token(symtab, ctxt); quote_tokens!(ctxt, Some($mx as usize)) },
                };
                quote_tokens!(ctxt, try!(xdr_codec::unpack_flex(input, $maxsz)))
            },

            _ => quote_tokens!(ctxt, try!(xdr_codec::Unpack::unpack(input))),
        }
    }

    fn as_token(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<Vec<rustast::TokenTree>> {
        use self::Type::*;

        let ret = match self {
            &Int        => quote_tokens!(ctxt, i32),
            &UInt       => quote_tokens!(ctxt, u32),
            &Hyper      => quote_tokens!(ctxt, i64),
            &UHyper     => quote_tokens!(ctxt, u64),
            &Float      => quote_tokens!(ctxt, f32),
            &Double     => quote_tokens!(ctxt, f64),
            &Quadruple  => quote_tokens!(ctxt, f128),
            &Bool       => quote_tokens!(ctxt, bool),

            &String     => quote_tokens!(ctxt, String),
            &Opaque     => quote_tokens!(ctxt, Vec<u8>),

            &Option(box ref ty) => {
                let tok = try!(ty.as_token(symtab, ctxt));
                if ty.is_boxed(symtab) {
                    quote_tokens!(ctxt, Option<Box<$tok>>)
                } else {
                    quote_tokens!(ctxt, Option<$tok>)
                }
            },

            &Array(box String, ref sz) | &Array(box Opaque, ref sz) => {
                let sztok = sz.as_token(symtab, ctxt);
                quote_tokens!(ctxt, [u8; $sztok as usize])
            },

            &Array(box ref ty, ref sz) => {
                let tytok = try!(ty.as_token(symtab, ctxt));
                let sztok = sz.as_token(symtab, ctxt);
                quote_tokens!(ctxt, [$tytok; $sztok as usize])
            },

            &Flex(box String, _) => quote_tokens!(ctxt, String),
            &Flex(box Opaque, _) => quote_tokens!(ctxt, Vec<u8>),
            &Flex(box ref ty, _) => {
                let tok = try!(ty.as_token(symtab, ctxt));
                quote_tokens!(ctxt, Vec<$tok>)
            },

            &Ident(ref name) => {
                let id = rustast::str_to_ident(name);
                quote_tokens!(ctxt, $id)
            },

            _ => return Err(Error::from(format!("can't have unnamed type {:?}", self))),
        };
        Ok(ret)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct EnumDefn(pub String, pub Option<Value>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct UnionCase(Value, Decl);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Decl {
    Void,
    Named(String, Type),
}

impl Decl {
    fn name_as_ident(&self) -> Option<(rustast::Ident, &Type)> {
        use self::Decl::*;
        match self {
            &Void => None,
            &Named(ref name, ref ty) => Some((rustast::str_to_ident(name), ty)),
        }
    }

    fn as_token(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<Option<(rustast::Ident, Vec<rustast::TokenTree>)>> {
        use self::Decl::*;
        use self::Type::*;
        match self {
            &Void => Ok(None),
            &Named(ref name, ref ty) => {
                let mut tok = try!(ty.as_token(symtab, ctxt));
                if false && ty.is_boxed(symtab) {
                    tok = quote_tokens!(ctxt, Box<$tok>)
                };
                Ok(Some((rustast::str_to_ident(name), tok)))
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Typespec(pub String, pub Type);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Const(pub String, pub i64);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Typesyn(pub String, pub Type);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Defn {
    Typespec(String, Type),
    Typesyn(String, Type),
    Const(String, i64),
}

pub trait Emit {
    fn define(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<rustast::P<rustast::Item>>;
}

pub trait Emitpack : Emit {
    fn pack(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<Option<rustast::P<rustast::Item>>>;
    fn unpack(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<Option<rustast::P<rustast::Item>>>;
}

impl Emit for Const {
    fn define(&self, _: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<rustast::P<rustast::Item>> {
        let name = rustast::str_to_ident(&self.0);
        let val = &self.1;

        Ok(quote_item!(ctxt, pub const $name: i64 = $val;).unwrap())
    }
}

impl Emit for Typesyn {
    fn define(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt)
              -> Result<rustast::P<rustast::Item>> {
        let ty = &self.1;
        let name = rustast::str_to_ident(&self.0);
        let tok = try!(ty.as_token(symtab, ctxt));
        Ok(quote_item!(ctxt, pub type $name = $tok;).unwrap())
    }
}

impl Emit for Typespec {
    fn define(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<rustast::P<rustast::Item>> {
        use self::Type::*;

        let name = rustast::str_to_ident(&self.0);
        let ty = &self.1;

        let ret = match ty {
            &Enum(ref edefs) => {
                let defs: Vec<_> = edefs.iter()
                    .filter_map(|&EnumDefn(ref field, _)| {
                        if let Some((val, Some(_))) = symtab.getconst(field) {
                            Some((rustast::str_to_ident(&field), val as isize))
                        } else {
                            None
                        }
                    })
                    .map(|(field, val)| quote_tokens!(ctxt, $field = $val,))
                    .collect();

                quote_item!(ctxt, #[derive(Debug, Eq, PartialEq, Copy, Clone)] pub enum $name { $defs }).unwrap()
            },

            &Struct(ref decls) => {
                use self::Decl::*;

                let decls: Vec<_> = try!(fold_result(
                    decls.iter()
                        .filter_map(|decl| result_option(decl.as_token(symtab, ctxt)))
                        .map(|res| res.map(|(field, ty)|
                                           quote_tokens!(ctxt, pub $field: $ty,)))));

                quote_item!(ctxt,
                            #[derive(Debug, Eq, PartialEq, Clone)]
                            pub struct $name { $decls }).unwrap()
            },

            &Union(box ref selector, ref cases, ref defl) => {
                use self::Decl::*;
                use self::Value::*;

                let labelfields = false; // true - include label in enum branch

                // return true if case is compatible with the selector
                let compatcase = |case: &Value| {
                    let seltype = match selector {
                        &Void => return false,
                        &Named(_, ref ty) => ty
                    };

                    match case {
                        &Const(val) if val < 0 =>
                            match seltype {
                                &Int | &Hyper => true,
                                _ => false,
                            },

                        &Const(_) =>
                            match seltype {
                                &Int | &Hyper | &UInt | &UHyper => true,
                                _ => false,
                            },

                        &Ident(ref id) =>
                            if *seltype == Bool {
                                id == "TRUE" || id == "FALSE"
                            } else {
                                if let &Type::Ident(ref selname) = seltype {
                                    match symtab.getconst(id) {
                                        Some((_, Some(ref scope))) => scope == selname,
                                        _ => false,
                                    }
                                } else {
                                    false
                                }
                            },
                    }
                };

                let mut cases: Vec<_> = try!(fold_result(
                    cases.iter()
                        .map(|&UnionCase(ref val, ref decl)| {
                            if !compatcase(val) {
                                return Err(Error::from(format!("incompat selector {:?} case {:?}", selector, val)));
                            }

                            let label = val.as_ident();

                            match decl {
                                &Void => Ok(quote_tokens!(ctxt, $label,)),
                                &Named(ref name, ref ty) => {
                                    let mut tok = try!(ty.as_token(symtab, ctxt));
                                    if false && ty.is_boxed(symtab) {
                                        tok = quote_tokens!(ctxt, Box<$tok>)
                                    };
                                    if labelfields {
                                        let name = rustast::str_to_ident(name);
                                        Ok(quote_tokens!(ctxt, $label { $name : $tok },))
                                    } else {
                                        Ok(quote_tokens!(ctxt, $label($tok),))
                                    }
                                }
                            }
                        })));

                if let &Some(ref def_val) = defl {
                    match *def_val {
                        box Named(ref name, ref ty) => {
                            let mut tok = try!(ty.as_token(symtab, ctxt));
                            if ty.is_boxed(symtab) {
                                tok = quote_tokens!(ctxt, Box<$tok>)
                            };
                            if labelfields {
                                let name = rustast::str_to_ident(name);
                                cases.push(quote_tokens!(ctxt, default { $name: $tok },))
                            } else {
                                cases.push(quote_tokens!(ctxt, default($tok),))
                            }
                        },
                        box Void => {
                            cases.push(quote_tokens!(ctxt, default,))
                        },
                    }
                }

                quote_item!(ctxt,
                            #[derive(Debug, Eq, PartialEq, Clone)]
                            pub enum $name { $cases }).unwrap()
            },

            _ => {
                let tok = try!(ty.as_token(symtab, ctxt));
                quote_item!(ctxt, pub type $name = $tok;).unwrap()
            },
        };
        Ok(ret)
    }
}

impl Emitpack for Typespec {
    fn pack(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<Option<rustast::P<rustast::Item>>> {
        use self::Type::*;
        use self::Decl::*;

        let name = rustast::str_to_ident(&self.0);
        let ty = &self.1;
        let mut directive = Vec::new();

        let body: Vec<rustast::TokenTree> = match ty {
            &Enum(_) => {
                directive = quote_tokens!(ctxt, #[inline]);
                try!(ty.packer(quote_tokens!(ctxt, self), symtab, ctxt))
            },

            &Struct(ref decl) => {
                let decls: Vec<_> = decl.iter()
                    .filter_map(|d| match d {
                        &Void => None,
                        &Named(ref name, ref ty) => Some((rustast::str_to_ident(name), ty)),
                    })
                    .map(|(field, ty)| {
                        let p = ty.packer(quote_tokens!(ctxt, self.$field), symtab, ctxt).unwrap();
                        quote_tokens!(ctxt, $p + )
                    })
                    .collect();
                quote_tokens!(ctxt, $decls 0)
            },

            &Union(_, ref cases, ref defl) => {
                let mut matches: Vec<_> = cases.iter()
                    .filter_map(|&UnionCase(ref val, ref decl)| {
                        let label = val.as_ident();
                        let disc = val.as_token(symtab, ctxt);

                        let ret = match decl {
                            &Void =>
                                quote_tokens!(ctxt, &$name::$label => try!($disc.pack(out)),),
                            &Named(_, ref ty) => {
                                let pack = match ty.packer(quote_tokens!(ctxt, val), symtab, ctxt) {
                                    Err(_) => return None,
                                    Ok(p) => p,
                                };
                                quote_tokens!(ctxt, &$name::$label(ref val) => try!($disc.pack(out)) + $pack,)
                            },
                        };
                        Some(ret)
                    })
                    .collect();

                if let &Some(box ref decl) = defl {
                    let default =
                        match decl {
                            &Void => quote_tokens!(ctxt, &$name::default => return Err(xdr_codec::Error::invalidcase()),),
                            &Named(_, _) => quote_tokens!(ctxt, &$name::default(_) => return Err(xdr_codec::Error::invalidcase()),),
                        };

                    matches.push(default)
                }

                quote_tokens!(ctxt, match self { $matches })
            },

            &Flex(box Opaque, _) =>
                quote_tokens!(ctxt, try!(xdr_codec::Opaque::borrowed(self).pack(out))),

            &Ident(_) => return Ok(None),

            _ => {
                if ty.is_prim(symtab) {
                    return Ok(None)
                } else {
                    try!(ty.packer(quote_tokens!(ctxt, self), symtab, ctxt))
                }
            },
        };

        trace!("body {:?}", body);

        Ok(quote_item!(ctxt,
                       impl<Out: xdr_codec::Write> xdr_codec::Pack<Out> for $name {
                           $directive
                               fn pack(&self, out: &mut Out) -> xdr_codec::Result<usize> {
                                   Ok($body)
                               }
                       }))
    }

    fn unpack(&self, symtab: &Symtab, ctxt: &rustast::ExtCtxt) -> Result<Option<rustast::P<rustast::Item>>> {
        use self::Type::*;
        use self::Decl::*;

        let name = rustast::str_to_ident(&self.0);
        let ty = &self.1;
        let mut directive = Vec::new();

        let body = match ty {
            &Enum(ref defs) => {
                directive = quote_tokens!(ctxt, #[inline]);
                let matchdefs: Vec<_> = defs.iter()
                    .filter_map(|&EnumDefn(ref name, _)| {
                        let tok = rustast::str_to_ident(name);
                        if let Some((ref _val, ref scope)) = symtab.getconst(name) {
                            //let val = *val as i32;
                            if let &Some(ref scope) = scope {
                                let scope = rustast::str_to_ident(scope);
                                //Some(quote_tokens!(ctxt, $val => $scope :: $tok,))
                                Some(quote_tokens!(ctxt, x if x == $scope :: $tok as i32 => $scope :: $tok,))
                            } else {
                                //Some(quote_tokens!(ctxt, $val => $tok,))
                                Some(quote_tokens!(ctxt, x if x == $tok as i32 => $tok,))
                            }
                        } else {
                            println!("unknown ident {}", name);
                            None
                        }
                    })
                    .collect();

                quote_tokens!(ctxt, {
                    let (e, esz): (i32, _) = try!(xdr_codec::Unpack::unpack(input));
                    sz += esz;
                    match e {
                        $matchdefs
                        _ => return Err(xdr_codec::Error::invalidenum())
                    }
                })
            },

            &Struct(ref decls) => {
                let decls: Vec<_> = decls.iter()
                    .filter_map(|decl| decl.name_as_ident())
                    .map(|(field, ty)| {
                        let unpack = ty.unpacker(symtab, ctxt);
                        quote_tokens!(ctxt,
                                      $field: { let (v, fsz) = $unpack; sz += fsz; v },)
                    })
                    .collect();

                quote_tokens!(ctxt, $name { $decls })
            },

            &Union(box ref sel, ref cases, ref defl) => {
                let mut matches: Vec<_> = try!(fold_result(
                    cases.iter()
                        .map(|&UnionCase(ref val, ref decl)| {
                            let label = val.as_ident();
                            let disc = match val.as_i64(symtab) {
                                Some(v) => v as i32,
                                None => return Err(Error::from(format!("disc val {:?} unknown", val))),
                            };

                            let ret = match decl {
                                //&Void => quote_tokens!(ctxt, $disc => $name::$label,),
                                &Void => quote_tokens!(ctxt, x if x == ($disc as i32) => $name::$label,),
                                &Named(_, ref ty) => {
                                    let unpack = ty.unpacker(symtab, ctxt);
                                    //quote_tokens!(ctxt, $disc => $name::$label({ let (v, fsz) = $unpack; sz += fsz; v }),)
                                    quote_tokens!(ctxt, x if x == ($disc as i32) => $name::$label({ let (v, fsz) = $unpack; sz += fsz; v }),)
                                },
                            };
                            Ok(ret)
                        })));

                if let &Some(box ref decl) = defl {
                    let defl = match decl {
                        &Void => quote_tokens!(ctxt, _ => $name::default),
                        &Named(_, ref ty) => {
                            let unpack = ty.unpacker(symtab, ctxt);
                            quote_tokens!(ctxt, _ => $name::default({
                                let (v, csz) = $unpack;
                                sz += csz;
                                v
                            }))
                        },
                    };

                    matches.push(defl);
                } else {
                    let defl = quote_tokens!(ctxt, _ => return Err(xdr_codec::Error::invalidcase()));
                    matches.push(defl);
                }

                let selunpack = match sel {
                    &Void => panic!("void switch selector?"),
                    &Named(_, ref ty) => ty.unpacker(symtab, ctxt),
                };

                quote_tokens!(ctxt, match { let (v, dsz): (i32, _) = $selunpack; sz += dsz; v } { $matches })
            },

            &Flex(_, _) | &Array(_, _) | &Option(_) => ty.unpacker(symtab, ctxt),

            &Ident(_) => return Ok(None),

            _ if ty.is_prim(symtab) => return Ok(None),
            _ => return Err(Error::from(format!("unimplemented ty={:?}", ty)))
        };

        Ok(quote_item!(ctxt,
                       impl<In: xdr_codec::Read> xdr_codec::Unpack<In> for $name {
                           $directive
                               fn unpack(input: &mut In) -> xdr_codec::Result<($name, usize)> {
                                   let mut sz = 0;
                                   Ok(($body, sz))
                               }
                       }))
    }
}

#[derive(Debug, Clone)]
pub struct Symtab {
    consts: BTreeMap<String, (i64, Option<String>)>,
    typespecs: BTreeMap<String, Type>,
    typesyns: BTreeMap<String, Type>,
}

impl Symtab {
    pub fn new(defns: &Vec<Defn>) -> Symtab {
        let mut ret = Symtab {
            consts: BTreeMap::new(),
            typespecs: BTreeMap::new(),
            typesyns: BTreeMap::new(),
        };

        ret.update_consts(&defns);

        ret
    }

    fn update_consts(&mut self, defns: &Vec<Defn>) {
        for defn in defns {
            match defn {
                &Defn::Typespec(ref name, ref ty) => {
                    self.deftype(name, ty);
                    self.update_enum_consts(name, ty);
                },

                &Defn::Const(ref name, val) => {
                    self.defconst(name, val, None)
                },

                &Defn::Typesyn(ref name, ref ty) => {
                    self.deftypesyn(name, ty);
                },
            }
        }
    }

    fn update_enum_consts(&mut self, scope: &String, ty: &Type) {
        let mut err = stderr();
        let mut prev = -1;

        if let &Type::Enum(ref edefn) = ty {
            for &EnumDefn(ref name, ref maybeval) in edefn {
                let v = match maybeval {
                    &None => prev + 1,
                    &Some(ref val) => match self.value(val) {
                        Some(c) => c,
                        None => { let _ = writeln!(&mut err, "Unknown value {:?}", val); continue }
                    }
                };

                prev = v;

                //println!("enum {} -> {}", name, v);
                self.defconst(name, v, Some(scope.clone()));
            }
        }
    }

    fn defconst(&mut self, name: &String, val: i64, scope: Option<String>) {
        self.consts.insert(name.clone(), (val, scope));
    }

    fn deftype(&mut self, name: &String, ty: &Type) {
        self.typespecs.insert(name.clone(), ty.clone());
    }

    fn deftypesyn(&mut self, name: &String, ty: &Type) {
        self.typesyns.insert(name.clone(), ty.clone());
    }

    pub fn getconst(&self, name: &String) -> Option<(i64, Option<String>)> {
        match self.consts.get(name) {
            None => None,
            Some(c) => Some(c.clone()),
        }
    }

    pub fn value(&self, val: &Value) -> Option<i64> {
        match val {
            &Value::Const(c) => Some(c),
            &Value::Ident(ref id) => self.getconst(id).map(|(v, _)| v),
        }
    }

    pub fn typespec(&self, name: &String) -> Option<&Type> {
        match self.typespecs.get(name) {
            None => match self.typesyns.get(name) {
                None => None,
                Some(ty) => Some(ty),
            },
            Some(ty) => Some(ty),
        }
    }

    pub fn constants(&self) -> Iter<String, (i64, Option<String>)> {
        self.consts.iter()
    }

    pub fn typespecs(&self) -> Iter<String, Type> {
        self.typespecs.iter()
    }

    pub fn typesyns(&self) -> Iter<String, Type> {
        self.typesyns.iter()
    }
}


#[cfg(test)]
mod test;
