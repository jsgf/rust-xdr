use std::io;
use std::io::{Write, Read};
use std::borrow::Borrow;
use std::error;
use std::result;
use std::string;
use std::fmt::{self,Display,Formatter};
use byteorder::{self, BigEndian, WriteBytesExt, ReadBytesExt};

use super::spec::ParseError;

pub type Result<T> = result::Result<T, Error>;
#[derive(Debug)]
pub enum Error {
    Byteorder(byteorder::Error),
    IOError(io::Error),
    InvalidUtf8(string::FromUtf8Error),
    InvalidCase,
    ParseErr(ParseError),
}

impl Error {
    pub fn invalidcase() -> Error {
        Error::InvalidCase
    }
    pub fn badutf8(err: string::FromUtf8Error) -> Error {
        Error::InvalidUtf8(err)
    }
    pub fn byteorder(berr: byteorder::Error) -> Error {
        match berr {
            byteorder::Error::Io(ioe) => Error::IOError(ioe),
            _ => Error::Byteorder(berr),
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self { Error::IOError(err) }
}

impl From<string::FromUtf8Error> for Error {
    fn from(err: string::FromUtf8Error) -> Self { Error::InvalidUtf8(err) }
}

impl From<byteorder::Error> for Error {
    fn from(err: byteorder::Error) -> Self {
        match err {
            byteorder::Error::Io(ioe) => Error::IOError(ioe),
            _ => Error::Byteorder(err),
        }
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self { Error::ParseErr(err) }
}

unsafe impl Send for Error {}
unsafe impl Sync for Error {}

impl error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::Byteorder(ref be) => be.description(),
            &Error::IOError(ref ioe) => ioe.description(),
            &Error::InvalidUtf8(ref se) => se.description(),
            &Error::ParseErr(ref pe) => pe.description(),
            &Error::InvalidCase => "invalid switch case",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match self {
            &Error::Byteorder(ref be) => Some(be),
            &Error::IOError(ref ioe) => Some(ioe),
            &Error::InvalidUtf8(ref se) => Some(se),
            _ => None
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> result::Result<(), fmt::Error> {
        use std::error::Error;
        write!(fmt, "{}", self.description())
    }
}

// return padding needed
#[inline]
fn padding(sz: usize) -> usize {
    (4 - (sz % 4)) % 4
}

pub fn pack<Out: Write, T: Pack<Out>>(val: &T, out: &mut Out) -> Result<()> {
    val.pack(out).map(|_| ())
}

pub trait Pack<Out: Write> {
    fn pack(&self, out: &mut Out) -> Result<usize>;
}

impl<Out: Write> Pack<Out> for u8 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_u8(*self).map_err(Error::from).map(|_| 1)
    }
}

impl<Out: Write> Pack<Out> for u32 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_u32::<BigEndian>(*self).map_err(Error::from).map(|_| 4)
    }

}

impl<Out: Write> Pack<Out> for i32 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_i32::<BigEndian>(*self).map_err(Error::from).map(|_| 4)
    }
}

impl<Out: Write> Pack<Out> for u64 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_u64::<BigEndian>(*self).map_err(Error::from).map(|_| 8)
    }
}

impl<Out: Write> Pack<Out> for i64 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_i64::<BigEndian>(*self).map_err(Error::from).map(|_| 8)
    }
}

impl<Out: Write> Pack<Out> for f32 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_f32::<BigEndian>(*self).map_err(Error::from).map(|_| 4)
    }
}

impl<Out: Write> Pack<Out> for f64 {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        out.write_f64::<BigEndian>(*self).map_err(Error::from).map(|_| 8)
    }
}

impl<Out: Write> Pack<Out> for bool {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        (*self as u32).pack(out)
    }
}

impl<Out: Write> Pack<Out> for () {
    #[inline]
    fn pack(&self, _out: &mut Out) -> Result<usize> {
        Ok(0)
    }
}

impl<Out: Write> Pack<Out> for usize {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        (*self as u32).pack(out)
    }
}

impl<Out: Write, T: Pack<Out>> Pack<Out> for [T] {
    fn pack(&self, out: &mut Out) -> Result<usize> {
        let len = self.len();
        
        let mut sz = try!(len.pack(out));
        for it in self {
            sz += try!(it.pack(out))
        }
        for _ in 0..padding(sz) {
            sz += try!(0u8.pack(out));
        }
        Ok(sz)
    }
}

impl<Out: Write, T: Pack<Out>> Pack<Out> for Vec<T> {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        (&self[..]).pack(out)
    }
}

impl<Out: Write> Pack<Out> for String {
    #[inline]
    fn pack(&self, out: &mut Out) -> Result<usize> {
        self.as_bytes().pack(out)
    }
}

impl<Out: Write, T: Pack<Out>> Pack<Out> for Option<T> {
    fn pack(&self, out: &mut Out) -> Result<usize> {
        match self {
            &None => false.pack(out),
            &Some(ref v) => {
                let sz = try!(true.pack(out)) + try!(v.pack(out));
                Ok(sz)
            }
        }
    }
}

impl<Out: Write, T: Pack<Out>> Pack<Out> for Box<T> {
    fn pack(&self, out: &mut Out) -> Result<usize> {
        let t: &T = self.borrow();
        t.pack(out)
    }
}

pub fn unpack<In: Read, T: Unpack<In>>(input: &mut In) -> Result<T> {
    T::unpack(input).map(|(v, _)| v)
}

pub trait Unpack<In: Read>: Sized {
    fn unpack(input: &mut In) -> Result<(Self, usize)>;
}

impl<In: Read> Unpack<In> for u8 {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_u8().map_err(Error::from).map(|v| (v, 1))
    }
}

impl<In: Read> Unpack<In> for u32 {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_u32::<BigEndian>().map_err(Error::from).map(|v| (v, 4))
    }
}

impl<In: Read> Unpack<In> for i32 {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_i32::<BigEndian>().map_err(Error::from).map(|v| (v, 4))
    }
}

impl<In: Read> Unpack<In> for u64 {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_u64::<BigEndian>().map_err(Error::from).map(|v| (v, 8))
    }
}

impl<In: Read> Unpack<In> for i64 {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_i64::<BigEndian>().map_err(Error::from).map(|v| (v, 8))
    }
}

impl<In: Read> Unpack<In> for f32 {
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_f32::<BigEndian>().map_err(Error::from).map(|v| (v, 4))
    }
}

impl<In: Read> Unpack<In> for f64 {
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        input.read_f64::<BigEndian>().map_err(Error::from).map(|v| (v, 8))
    }
}

impl<In: Read> Unpack<In> for bool {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        u32::unpack(input).map(|(v, sz)| (v != 0, sz))
    }
}

impl<In: Read> Unpack<In> for usize {
    #[inline]
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        u32::unpack(input).map(|(v, sz)| (v as usize, sz))
    }
}

impl<In: Read, T: Unpack<In>> Unpack<In> for Vec<T> {
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        let (elems, mut sz) = try!(Unpack::unpack(input));
        let mut out = Vec::with_capacity(elems);
        
        for _ in 0..elems {
            let (e, esz) = try!(Unpack::unpack(input));
            out.push(e);
            sz += esz;
        }
        for _ in 0..padding(sz) {
            let (_, psz): (u8, _) = try!(Unpack::unpack(input));
            sz += psz;
        }

        Ok((out, sz))
    }
}

impl<In: Read> Unpack<In> for String {
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        let (v, sz) = try!(Unpack::unpack(input));
        String::from_utf8(v).map_err(Error::from).map(|s| (s, sz))
    }    
}

impl<In: Read, T: Unpack<In>> Unpack<In> for Option<T> {
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        let (have, mut sz) = try!(Unpack::unpack(input));
        let ret = if have {
            let (v, osz) = try!(Unpack::unpack(input));
            sz += osz;
            Some(v)
        } else {
            None
        };
        Ok((ret, sz))
    }
}

impl<In: Read, T: Unpack<In>> Unpack<In> for Box<T> {
    fn unpack(input: &mut In) -> Result<(Self, usize)> {
        let (b, sz) = try!(Unpack::unpack(input));
        Ok((Box::new(b), sz))
    }
}
