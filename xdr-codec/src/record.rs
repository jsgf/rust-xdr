//! XDR record marking
//!
//! This module implements wrappers for `Write` and `BufRead` which
//! implement "Record Marking" from RFC1831, used for encoding XDR
//! structures onto a bytestream such as TCP.
//!
//! The format is simple - each record is broken up into one or more
//! record fragments. Each record fragment is prefixed with a 32-bit
//! big-endian value. The low 31 bits is the fragment size, and the
//! top bit is the "end of record" marker, indicating the last
//! fragment of the record.
//!
//! There's no magic number or other way to determine whether a stream
//! is using record marking; both ends must agree.
use std::io::{self, Read, BufRead, Write};
use std::cmp::min;
use byteorder;

use super::{unpack, pack, Error};

const LAST_REC: u32 = 1u32 << 31;

fn mapioerr(xdrerr: Error) -> io::Error {
    match xdrerr {
        Error::IOError(ioerr) => ioerr,
        other => io::Error::new(io::ErrorKind::Other, other),
    }
}

/// Read records from a bytestream.
///
/// Reads will read up to the end of the current fragment, and not
/// beyond. The `BufRead` trait doesn't otherwise allow for record
/// boundaries to be deliniated. Callers can use the `eor` method to
/// determine record ends.
pub struct XdrRecordReader<R: BufRead> {
    size: usize,                // record size
    consumed: usize,            // bytes consumed
    eor: bool,                  // is last record

    reader: R,                  // reader
}

impl<R: BufRead> XdrRecordReader<R> {
    /// Wrapper a record reader around an existing implementation of
    /// `BufRead`, such as `BufReader`.
    pub fn new(rd: R) -> XdrRecordReader<R> {
        XdrRecordReader {
            size: 0,
            consumed: 0,
            eor: false,
            reader: rd
        }
    }

    fn nextrec(&mut self) -> io::Result<bool> {
        assert_eq!(self.consumed, self.size);

        let rechdr: u32 =
            match unpack(&mut self.reader) {
                Ok(v) => v,
                Err(Error::Byteorder(byteorder::Error::UnexpectedEOF)) => return Ok(true),
                Err(e) => return Err(mapioerr(e)),
            };

        self.size = (rechdr & !LAST_REC) as usize;
        self.consumed = 0;
        self.eor = (rechdr & LAST_REC) != 0;

        Ok(false)
    }

    fn totremains(&self) -> usize { self.size - self.consumed }

    /// Current fragment is the end of the record.
    pub fn eor(&self) -> bool {
        self.eor
    }
}

impl<R: BufRead> Read for XdrRecordReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let nread = {
            let data = try!(self.fill_buf());
            let len = min(buf.len(), data.len());

            try!((&data[..len]).read(buf))
        };

        self.consume(nread);
        Ok(nread)
    }
}

impl<R: BufRead> BufRead for XdrRecordReader<R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        while self.totremains() == 0 {
            if try!(self.nextrec()) {
                return Ok(&[])
            }
        }

        let remains = self.totremains();
        let data = try!(self.reader.fill_buf());
        Ok(&data[..min(data.len(), remains)])
    }

    fn consume(&mut self, sz: usize) {
        assert!(sz <= self.totremains());
        self.consumed += sz;
        self.reader.consume(sz);
    }
}

const WRBUF: usize = 65536;

/// Write records into a bytestream.
///
/// Flushes the current buffer as end of record when destroyed.
pub struct XdrRecordWriter<W: Write> {
    buf: Vec<u8>,
    bufsz: usize,
    writer: W,
}

impl<W: Write> XdrRecordWriter<W> {
    /// Create a new `XdrRecordWriter` wrapped around a `Write`
    /// implementation, using a default buffer size (64k).
    pub fn new(w: W) -> XdrRecordWriter<W> {
        XdrRecordWriter::with_buffer(w, WRBUF)
    }

    /// Create an instance with a specific buffer size. Panics if the
    /// size is zero.
    pub fn with_buffer(w: W, bufsz: usize) -> XdrRecordWriter<W> {
        if bufsz == 0 { panic!("bufsz must be non-zero") }
        XdrRecordWriter {
            buf: Vec::with_capacity(bufsz),
            bufsz: bufsz,
            writer: w
        }
    }

    /// Flush the current buffer. If `eor` is true, the end of record
    /// marker is set.
    pub fn flush_eor(&mut self, eor: bool) -> io::Result<()> {
        if !eor && self.buf.len() == 0 { return Ok(()) }

        let mut rechdr = self.buf.len() as u32;
        if eor { rechdr |= LAST_REC };

        try!(pack(&rechdr, &mut self.writer).map_err(mapioerr));
        let _ = try!(self.writer.write_all(&self.buf).map(|_| ()));
        self.buf.truncate(0);

        self.writer.flush()
    }
}

impl<W: Write> Drop for XdrRecordWriter<W> {
    fn drop(&mut self) {
        let _ = self.flush_eor(true);
    }
}

impl<W: Write> Write for XdrRecordWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut off = 0;

        while off < buf.len() {
            let chunk = &buf[off..off+min(buf.len() - off, self.bufsz)];
            if self.buf.len() + chunk.len() > self.bufsz {
                try!(self.flush())
            }

            self.buf.extend(chunk);
            off += chunk.len();
        }

        Ok(off)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_eor(false)
    }   
}

#[cfg(test)]
mod test {
    use std::io::{Read, Write, Cursor};
    use super::*;

    #[test]
    fn recread_full() {
        let inbuf = vec![128, 0, 0, 10,  0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let cur = Cursor::new(inbuf);

        let mut recread = XdrRecordReader::new(cur);
        let mut buf = vec![0; 20];

        assert_eq!(recread.read(&mut buf[..]).unwrap(), 10);
        assert_eq!(buf, vec![0,1,2,3,4,5,6,7,8,9, 0,0,0,0,0,0,0,0,0,0]);
        assert!(recread.eor());
    }

    #[test]
    fn recread_short() {
        let inbuf = vec![128, 0, 0, 10,  0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
        let cur = Cursor::new(inbuf);

        let mut recread = XdrRecordReader::new(cur);
        let mut buf = vec![0; 5];

        assert_eq!(recread.read(&mut buf[..]).unwrap(), 5);
        assert!(recread.eor());
        assert_eq!(buf, vec![0,1,2,3,4]);

        assert_eq!(recread.read(&mut buf[..]).unwrap(), 5);
        assert!(recread.eor());
        assert_eq!(buf, vec![5,6,7,8,9]);
    }

    #[test]
    fn recread_half() {
        let inbuf = vec![  0, 0, 0, 5,  0, 1, 2, 3, 4,
                           128, 0, 0, 5,  5, 6, 7, 8, 9];
        let cur = Cursor::new(inbuf);

        let mut recread = XdrRecordReader::new(cur);
        let mut buf = vec![0; 10];

        assert_eq!(recread.read(&mut buf[..]).unwrap(), 5);
        assert_eq!(buf, vec![0,1,2,3,4, 0,0,0,0,0]);
        assert!(!recread.eor());

        assert_eq!(recread.read(&mut buf[..]).unwrap(), 5);
        assert_eq!(buf, vec![5,6,7,8,9, 0,0,0,0,0]);
        assert!(recread.eor());
    }

    #[test]
    fn read_zerorec() {
        let inbuf = vec![0, 0, 0, 0,
                         0, 0, 0, 0,
                         128, 0, 0, 0];

        let cur = Cursor::new(inbuf);
        let mut recread = XdrRecordReader::new(cur);

        let mut buf = [0; 100];
        assert_eq!(recread.read(&mut buf).unwrap(), 0);
        assert!(recread.eor());
    }

    #[test]
    #[should_panic(expected="must be non-zero")]
    fn zerosz() {
        let buf = Vec::new();
        let _ = XdrRecordWriter::with_buffer(buf, 0);
    }

    #[test]
    fn smallrec() {
        let mut buf = Vec::new();

        {
            let mut xw = XdrRecordWriter::new(&mut buf);

            assert_eq!(write!(xw, "hello").unwrap(), ());
        }

        assert_eq!(buf, vec![128, 0, 0, 5,  104, 101, 108, 108, 111])
    }

    #[test]
    fn largerec() {
        let mut buf = Vec::new();

        {
            let mut xw = XdrRecordWriter::with_buffer(&mut buf, 3);

            assert_eq!(write!(xw, "hello").unwrap(), ());
        }

        assert_eq!(buf, vec![  0, 0, 0, 3,  104, 101, 108,
                               128, 0, 0, 2,  108, 111])
    }

    #[test]
    fn largerec_flush() {
        let mut buf = Vec::new();

        {
            let mut xw = XdrRecordWriter::with_buffer(&mut buf, 10);

            assert_eq!(write!(xw, "hel").unwrap(), ());
            xw.flush().unwrap();
            assert_eq!(write!(xw, "lo").unwrap(), ());
            xw.flush().unwrap();
        }

        assert_eq!(buf, vec![  0, 0, 0, 3,  104, 101, 108,
                               0, 0, 0, 2,  108, 111,
                               128, 0, 0, 0])
    }
}
