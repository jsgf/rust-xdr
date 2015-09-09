use std::io::{self, Read, BufRead, Write};
use std::cmp::min;

use super::{unpack, pack, Result, Error};

const LAST_REC: u32 = 1u32 << 31;

struct Record {
    size: usize,                // total size of record
    consumed: usize,            // amount of record consumed
    eor: bool,
}

impl Record {
    fn new<R: Read>(rd: &mut R) -> io::Result<Record> {
        let rechdr: u32 = try!(unpack(rd).map_err(mapioerr));
        Ok(Record {
                size: (rechdr & !LAST_REC) as usize,
                consumed: 0,
                eor: (rechdr & LAST_REC) != 0,
        })
    }

    fn totremains(&self) -> usize { self.size - self.consumed }

    fn consume(&mut self, sz: usize) {
        assert!(sz <= self.totremains());
        self.consumed += sz;
    }
}
pub struct XdrRecordReader<R: BufRead> {
    record: Option<Record>,
    reader: R,
}

impl<R: BufRead> XdrRecordReader<R> {
    pub fn new(rd: R) -> XdrRecordReader<R> {
        XdrRecordReader {
            record: None,
            reader: rd
        }
    }

    pub fn eor(&self) -> bool {
        if let Some(Record { eor, .. }) = self.record {
            eor
        } else {
            true
        }
    }
}

fn mapioerr(xdrerr: Error) -> io::Error {
    match xdrerr {
        Error::IOError(ioerr) => ioerr,
        other => io::Error::new(io::ErrorKind::Other, other),
    }
}

impl<R: BufRead> Read for XdrRecordReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut record = match self.record.take() {
            None => try!(Record::new(&mut self.reader)),
            Some(r) =>
                if r.totremains() == 0 {
                    try!(Record::new(&mut self.reader))
                } else {
                    r
                },
        };

        assert!(record.totremains() > 0);

        let nread = {
            let data = try!(self.reader.fill_buf());
            let len = min(data.len(), record.totremains());

            try!((&data[..len]).read(buf))
        };

        self.reader.consume(nread);
        record.consume(nread);

        self.record = Some(record);

        Ok(nread)
    }
}

const WRBUF: usize = 65536;

pub struct XdrRecordWriter<W: Write> {
    buf: Vec<u8>,
    bufsz: usize,
    writer: W,
}

impl<W: Write> XdrRecordWriter<W> {
    pub fn with_buffer(w: W, bufsz: usize) -> XdrRecordWriter<W> {
        XdrRecordWriter {
            buf: Vec::with_capacity(bufsz),
            bufsz: bufsz,
            writer: w
        }
    }

    pub fn new(w: W) -> XdrRecordWriter<W> {
        XdrRecordWriter::with_buffer(w, WRBUF)
    }
    
    pub fn flush_eor(&mut self, eor: bool) -> io::Result<()> {
        if !eor && self.buf.len() == 0 { return Ok(()) }

        let mut rechdr = self.buf.len() as u32;
        if eor { rechdr |= LAST_REC };

        try!(pack(&rechdr, &mut self.writer).map_err(mapioerr));
        let _ = try!(self.writer.write_all(&self.buf).map(|_| ()));
        self.buf.truncate(0);
        Ok(())
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
    use std::io::{Read, Write};
    use super::*;

    #[test]
    fn recread_full() {
        use std::io::Cursor;

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
        use std::io::Cursor;

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
        use std::io::Cursor;

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
