use std::fmt;

use lazy_regex::{regex, regex_replace_all};
use oem_cp::{
    code_table::{DECODING_TABLE_CP737, ENCODING_TABLE_CP737},
    decode_string_complete_table, encode_string_checked,
};

#[derive(Clone, PartialEq)]
pub struct AsciiStr {
    inner: Vec<u8>,
}

impl AsciiStr {
    const WHITESPACE: [u8; 3] = [0x00, 0x20, 0xFF];

    pub fn new(bytes: Vec<u8>) -> Self {
        AsciiStr { inner: bytes }
    }

    pub unsafe fn from_bytes_unchecked<T: Iterator<Item = u8>>(buf: T) -> Self {
        AsciiStr {
            inner: buf.collect(),
        }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.inner
    }

    pub fn trim(&self) -> Self {
        match self
            .inner
            .iter()
            .position(|byte| !AsciiStr::WHITESPACE.contains(byte))
        {
            Some(i) => {
                // SAFETY: if there is a non-whitespace character in `position`, there must be one found in `rposition`
                let end = self
                    .inner
                    .iter()
                    .rposition(|byte| !AsciiStr::WHITESPACE.contains(byte))
                    .unwrap();

                AsciiStr {
                    inner: self.inner[i..=end].to_vec(),
                }
            }
            None => AsciiStr { inner: Vec::new() },
        }
    }
}

impl TryFrom<String> for AsciiStr {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if !value.is_ascii() {
            return Err(());
        }

        Ok(AsciiStr {
            inner: value.into_bytes(),
        })
    }
}

impl std::fmt::Debug for AsciiStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", unsafe {
            decode_string_complete_table(&self.inner, &DECODING_TABLE_CP737)
        })
    }
}

impl fmt::Display for AsciiStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe {
            decode_string_complete_table(&self.inner, &DECODING_TABLE_CP737)
        })
    }
}

impl PartialEq<str> for AsciiStr {
    fn eq(&self, other: &str) -> bool {
        decode_string_complete_table(&self.inner, &DECODING_TABLE_CP737) == other
    }
}

impl PartialEq<&str> for AsciiStr {
    fn eq(&self, other: &&str) -> bool {
        decode_string_complete_table(&self.inner, &DECODING_TABLE_CP737) == *other
    }
}

impl std::ops::Deref for AsciiStr {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl IntoIterator for AsciiStr {
    type Item = u8;
    type IntoIter = std::vec::IntoIter<u8>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnescapeError {
    UnmatchedBackslash(usize),
    InvalidAscii,
}

pub fn unescape_str<'a>(s: &'a str) -> Result<AsciiStr, UnescapeError> {
    let mut numbered = regex_replace_all!(r"\\x[0-9a-fA-F]{2}", s, |cap: &str| {
        let byte = u8::from_str_radix(cap.strip_prefix("\\x").unwrap(), 16).unwrap();
        unsafe { String::from_utf8_unchecked(vec![byte]) }
    });

    let owned = numbered.to_owned();
    numbered = regex_replace_all!(r"\\o[0-7]{3}", &owned, |oct: &str| {
        // The Regex expression guarantees a valid octal.
        let byte = u8::from_str_radix(oct.strip_prefix("\\o").unwrap(), 8).unwrap();

        unsafe { String::from_utf8_unchecked(vec![byte]) }
    });

    if let Some(invalid) = regex!(r"(\\[^nt0rabfv\\])|(\\\z)").find(&numbered) {
        return Err(UnescapeError::UnmatchedBackslash(invalid.start()));
    }

    let simple = numbered
        .replace("\\n", "\n")
        .replace("\\\\", "\\")
        .replace("\\t", "\t")
        .replace("\\'", "'")
        .replace("\\\"", "\"")
        .replace("\\0", "\0")
        .replace("\\r", "\r")
        .replace("\\a", "\x07")
        .replace("\\b", "\x08")
        .replace("\\f", "\x0C")
        .replace("\\v", "\x0B");

    let mut string =
        encode_string_checked(simple, &ENCODING_TABLE_CP737).ok_or(UnescapeError::InvalidAscii)?;
    string.push(0x00);

    Ok(AsciiStr::new(string))
}

mod tests {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn hello() {
        let test_str = "hello world!";
        let ascii = unescape_str(test_str).unwrap();
        assert_eq!(ascii, "hello world!\0");
    }

    #[test]
    fn unescape() {
        let test_str = "\\o050 hello \\x29 \\t\\n";
        let unescaped = unescape_str(test_str).unwrap();
        assert_eq!(unescaped, "\x28 hello \x29 \t\n\0");

        let failure = "\\050 \\";
        unescape_str(failure).unwrap_err();
    }
}
