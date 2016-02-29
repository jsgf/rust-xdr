extern crate xdr_codec;
extern crate quickcheck;

use std::io::Cursor;
use std::fmt::Debug;

use xdr_codec::{Pack, Unpack, Error};
use quickcheck::{quickcheck, Arbitrary};

// Output of packing is a multiple of 4
fn pack<T>(v: T) -> bool
    where T: PartialEq + Pack<Cursor<Vec<u8>>>
{
    let mut data = Cursor::new(Vec::new());

    let sz = v.pack(&mut data).expect("pack failed");
    sz % 4 == 0
}

// Packing something then unpacking returns the same value
fn codec<T>(v: T) -> bool
    where T: PartialEq + Pack<Cursor<Vec<u8>>> + Unpack<Cursor<Vec<u8>>>
 {
    let mut data = Cursor::new(Vec::new());

    let psz = v.pack(&mut data).expect("pack failed");

    let mut data = Cursor::new(data.into_inner());
    let (uv, usz) = T::unpack(&mut data).expect("unpack failed");

    psz == usz && v == uv
}

// Packing something then unpacking returns the same value
fn short_unpack<T>(v: T) -> bool
    where T: PartialEq + Pack<Cursor<Vec<u8>>> + Unpack<Cursor<Vec<u8>>>
 {
    let mut data = Cursor::new(Vec::new());

    let psz = v.pack(&mut data).expect("pack failed");

    // truncate data to make sure unpacking fails
    let data = data.into_inner();
    assert_eq!(psz, data.len());
    let data = Vec::from(&data[..data.len()-1]);

    let mut data = Cursor::new(data);
    match T::unpack(&mut data) {
        Err(Error::Byteorder(_)) => true,
        _ => false,
    }
}

fn quickcheck_pack_t<T>()
    where T: PartialEq + Pack<Cursor<Vec<u8>>> + Unpack<Cursor<Vec<u8>>> + Arbitrary + Debug
{
    quickcheck(pack as fn(T) -> bool);
    quickcheck(pack as fn(Vec<T>) -> bool);
    quickcheck(pack as fn(Option<T>) -> bool);
    quickcheck(pack as fn(Vec<Option<T>>) -> bool);
    quickcheck(pack as fn(Option<Vec<T>>) -> bool);
}

fn quickcheck_codec_t<T>()
    where T: PartialEq + Pack<Cursor<Vec<u8>>> + Unpack<Cursor<Vec<u8>>> + Arbitrary + Debug
{
    quickcheck(codec as fn(T) -> bool);
    quickcheck(codec as fn(Vec<T>) -> bool);
    quickcheck(codec as fn(Option<T>) -> bool);
    quickcheck(codec as fn(Vec<Option<T>>) -> bool);
    quickcheck(codec as fn(Option<Vec<T>>) -> bool);
}

fn quickcheck_short_unpack_t<T>()
    where T: PartialEq + Pack<Cursor<Vec<u8>>> + Unpack<Cursor<Vec<u8>>> + Arbitrary + Debug
{
    quickcheck(short_unpack as fn(T) -> bool);
    quickcheck(short_unpack as fn(Vec<T>) -> bool);
    quickcheck(short_unpack as fn(Option<T>) -> bool);
    quickcheck(short_unpack as fn(Vec<Option<T>>) -> bool);
    quickcheck(short_unpack as fn(Option<Vec<T>>) -> bool);
}

#[test]
fn quickcheck_pack_iu8() {
    // Special case i8/u8 since they're not packed.
    quickcheck(pack as fn(Vec<i8>) -> bool);
    quickcheck(pack as fn(Vec<u8>) -> bool);
}

#[test]
fn quickcheck_pack_ui32() {
    quickcheck_pack_t::<i32>();
    quickcheck_pack_t::<u32>();
    quickcheck_pack_t::<usize>();
}

#[test]
fn quickcheck_pack_iu64() {
    quickcheck_pack_t::<i64>();
    quickcheck_pack_t::<u64>();
}

#[test]
fn quickcheck_pack_float() {
    quickcheck_pack_t::<f32>();
    quickcheck_pack_t::<f64>();
}

#[test]
fn quickcheck_codec_iu8() {
    quickcheck_codec_t::<i8>();
    quickcheck_codec_t::<u8>();
}

#[test]
fn quickcheck_codec_ui32() {
    quickcheck_codec_t::<i32>();
    quickcheck_codec_t::<u32>();
    quickcheck_codec_t::<usize>();
}

#[test]
fn quickcheck_codec_iu64() {
    quickcheck_codec_t::<i64>();
    quickcheck_codec_t::<u64>();
}

#[test]
fn quickcheck_codec_float() {
    quickcheck_codec_t::<f32>();
    quickcheck_codec_t::<f64>();
}

#[test]
fn quickcheck_short_unpack_iu8() {
    quickcheck_short_unpack_t::<i8>();
    quickcheck_short_unpack_t::<u8>();
}

#[test]
fn quickcheck_short_unpack_ui32() {
    quickcheck_short_unpack_t::<i32>();
    quickcheck_short_unpack_t::<u32>();
    quickcheck_short_unpack_t::<usize>();
}

#[test]
fn quickcheck_short_unpack_iu64() {
    quickcheck_short_unpack_t::<i64>();
    quickcheck_short_unpack_t::<u64>();
}

#[test]
fn quickcheck_short_unpack_float() {
    quickcheck_short_unpack_t::<f32>();
    quickcheck_short_unpack_t::<f64>();
}
