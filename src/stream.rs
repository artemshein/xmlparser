use core::char;
use core::cmp;
use core::ops::Range;
use core::str;

use crate::{StrSpan, StreamError, TextPos, XmlByteExt, XmlCharExt};

type Result<T> = ::core::result::Result<T, StreamError>;

// ASCII name bytes excluding ':' (handled separately as the qname splitter).
const fn build_name_class() -> [bool; 256] {
    let mut t = [false; 256];
    let mut i = 0;
    while i < 256 {
        let b = i as u8;
        t[i] = b.is_ascii_alphanumeric() || b == b'_' || b == b'-' || b == b'.';
        i += 1;
    }
    t
}
static NAME_CLASS: [bool; 256] = build_name_class();

// Bytes that can appear in character data without further checks.
// Excluded: controls other than \t \n \r, '<' (terminator), ']' (']]>' check),
// 0xEF (may start U+FFFE/U+FFFF). All other UTF-8 bytes are valid XML chars,
// since `&str` already guarantees valid UTF-8 (no surrogates).
const fn build_text_ok() -> [bool; 256] {
    let mut t = [true; 256];
    let mut i = 0;
    while i < 0x20 {
        t[i] = false;
        i += 1;
    }
    t[b'\t' as usize] = true;
    t[b'\n' as usize] = true;
    t[b'\r' as usize] = true;
    t[b'<' as usize] = false;
    t[b']' as usize] = false;
    t[0xEF] = false;
    t
}
static TEXT_OK: [bool; 256] = build_text_ok();

// Bytes that can appear in comment text without further checks.
// Excluded: controls other than \t \n \r, '-' (terminator and '--'
// detection), 0xEF (may start U+FFFE/U+FFFF).
const fn build_comment_ok() -> [bool; 256] {
    let mut t = [true; 256];
    let mut i = 0;
    while i < 0x20 {
        t[i] = false;
        i += 1;
    }
    t[b'\t' as usize] = true;
    t[b'\n' as usize] = true;
    t[b'\r' as usize] = true;
    t[b'-' as usize] = false;
    t[0xEF] = false;
    t
}
static COMMENT_OK: [bool; 256] = build_comment_ok();

/// Representation of the [Reference](https://www.w3.org/TR/xml/#NT-Reference) value.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Reference<'a> {
    /// An entity reference.
    ///
    /// <https://www.w3.org/TR/xml/#NT-EntityRef>
    Entity(&'a str),

    /// A character reference.
    ///
    /// <https://www.w3.org/TR/xml/#NT-CharRef>
    Char(char),
}

/// A streaming XML parsing interface.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Stream<'a> {
    pos: usize,
    end: usize,
    span: StrSpan<'a>,
}

impl<'a> From<&'a str> for Stream<'a> {
    #[inline]
    fn from(text: &'a str) -> Self {
        Stream {
            pos: 0,
            end: text.len(),
            span: text.into(),
        }
    }
}

impl<'a> From<StrSpan<'a>> for Stream<'a> {
    #[inline]
    fn from(span: StrSpan<'a>) -> Self {
        Stream {
            pos: 0,
            end: span.as_str().len(),
            span,
        }
    }
}

impl<'a> Stream<'a> {
    /// Creates a new stream from a specified `text` substring.
    #[inline]
    pub fn from_substr(text: &'a str, fragment: Range<usize>) -> Self {
        Stream {
            pos: fragment.start,
            end: fragment.end,
            span: text.into(),
        }
    }

    /// Returns an underling string span.
    #[inline]
    pub fn span(&self) -> StrSpan<'a> {
        self.span
    }

    /// Returns current position.
    #[inline]
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Sets current position equal to the end.
    ///
    /// Used to indicate end of parsing on error.
    #[inline]
    pub fn jump_to_end(&mut self) {
        self.pos = self.end;
    }

    /// Checks if the stream is reached the end.
    ///
    /// Any [`pos()`] value larger than original text length indicates stream end.
    ///
    /// Accessing stream after reaching end via safe methods will produce
    /// an `UnexpectedEndOfStream` error.
    ///
    /// Accessing stream after reaching end via *_unchecked methods will produce
    /// a Rust's bound checking error.
    ///
    /// [`pos()`]: #method.pos
    #[inline]
    pub fn at_end(&self) -> bool {
        self.pos >= self.end
    }

    /// Returns a byte from a current stream position.
    ///
    /// # Errors
    ///
    /// - `UnexpectedEndOfStream`
    #[inline]
    pub fn curr_byte(&self) -> Result<u8> {
        if self.at_end() {
            return Err(StreamError::UnexpectedEndOfStream);
        }

        Ok(self.curr_byte_unchecked())
    }

    /// Returns a byte from a current stream position.
    ///
    /// # Panics
    ///
    /// - if the current position is after the end of the data
    #[inline]
    pub fn curr_byte_unchecked(&self) -> u8 {
        self.span.as_bytes()[self.pos]
    }

    /// Returns a next byte from a current stream position.
    ///
    /// # Errors
    ///
    /// - `UnexpectedEndOfStream`
    #[inline]
    pub fn next_byte(&self) -> Result<u8> {
        if self.pos + 1 >= self.end {
            return Err(StreamError::UnexpectedEndOfStream);
        }

        Ok(self.span.as_bytes()[self.pos + 1])
    }

    /// Advances by `n` bytes.
    ///
    /// Advancing past the end is checked only by a `debug_assert!`;
    /// accessing the stream afterwards will panic on the out-of-bounds read.
    ///
    /// # Examples
    ///
    /// ```rust,should_panic
    /// use xmltok::Stream;
    ///
    /// let mut s = Stream::from("text");
    /// s.advance(2); // ok
    /// s.advance(20); // past the end; panics via debug_assert!() in debug builds
    /// s.curr_byte_unchecked(); // panics: index out of bounds
    /// ```
    #[inline]
    pub fn advance(&mut self, n: usize) {
        debug_assert!(self.pos + n <= self.end);
        self.pos += n;
    }

    /// Checks that the stream starts with a selected text.
    ///
    /// We are using `&[u8]` instead of `&str` for performance reasons.
    ///
    /// # Examples
    ///
    /// ```
    /// use xmltok::Stream;
    ///
    /// let mut s = Stream::from("Some text.");
    /// s.advance(5);
    /// assert_eq!(s.starts_with(b"text"), true);
    /// assert_eq!(s.starts_with(b"long"), false);
    /// ```
    #[inline]
    pub fn starts_with(&self, text: &[u8]) -> bool {
        self.span.as_bytes()[self.pos..self.end].starts_with(text)
    }

    /// Consumes the current byte if it's equal to the provided byte.
    ///
    /// # Errors
    ///
    /// - `InvalidChar`
    /// - `UnexpectedEndOfStream`
    ///
    /// # Examples
    ///
    /// ```
    /// use xmltok::Stream;
    ///
    /// let mut s = Stream::from("Some text.");
    /// assert!(s.consume_byte(b'S').is_ok());
    /// assert!(s.consume_byte(b'o').is_ok());
    /// assert!(s.consume_byte(b'm').is_ok());
    /// assert!(s.consume_byte(b'q').is_err());
    /// ```
    pub fn consume_byte(&mut self, c: u8) -> Result<()> {
        let curr = self.curr_byte()?;
        if curr != c {
            return Err(StreamError::InvalidChar(curr, c, self.gen_text_pos()));
        }

        self.advance(1);
        Ok(())
    }

    /// Tries to consume the current byte if it's equal to the provided byte.
    ///
    /// Unlike `consume_byte()` will not return any errors.
    pub fn try_consume_byte(&mut self, c: u8) -> bool {
        match self.curr_byte() {
            Ok(b) if b == c => {
                self.advance(1);
                true
            }
            _ => false,
        }
    }

    /// Skips selected string.
    ///
    /// # Errors
    ///
    /// - `InvalidString`
    pub fn skip_string(&mut self, text: &'static [u8]) -> Result<()> {
        if !self.starts_with(text) {
            let pos = self.gen_text_pos();

            // Assume that all input `text` are valid UTF-8 strings, so unwrap is safe.
            let expected = str::from_utf8(text).unwrap();

            return Err(StreamError::InvalidString(expected, pos));
        }

        self.advance(text.len());
        Ok(())
    }

    /// Consumes bytes by the predicate and returns them.
    ///
    /// The result can be empty.
    #[inline]
    pub fn consume_bytes<F>(&mut self, f: F) -> StrSpan<'a>
    where
        F: Fn(&Stream, u8) -> bool,
    {
        let start = self.pos;
        self.skip_bytes(f);
        self.slice_back(start)
    }

    /// Skips bytes by the predicate.
    #[inline]
    pub fn skip_bytes<F>(&mut self, f: F)
    where
        F: Fn(&Stream, u8) -> bool,
    {
        let bytes = self.span.as_bytes();
        let end = self.end;
        while self.pos < end && f(self, bytes[self.pos]) {
            self.pos += 1;
        }
    }

    /// Consumes chars by the predicate and returns them.
    ///
    /// The result can be empty.
    #[inline]
    pub fn consume_chars<F>(&mut self, f: F) -> Result<StrSpan<'a>>
    where
        F: Fn(&Stream, char) -> bool,
    {
        let start = self.pos;
        self.skip_chars(f)?;
        Ok(self.slice_back(start))
    }

    /// Skips chars by the predicate.
    #[inline]
    pub fn skip_chars<F>(&mut self, f: F) -> Result<()>
    where
        F: Fn(&Stream, char) -> bool,
    {
        let bytes = self.span.as_bytes();
        let end = self.end;
        while self.pos < end {
            let b = bytes[self.pos];
            if b < 128 {
                let c = b as char;
                if !c.is_xml_char() {
                    return Err(StreamError::NonXmlChar(c, self.gen_text_pos()));
                }
                if f(self, c) {
                    self.pos += 1;
                } else {
                    break;
                }
            } else {
                for c in self.span.as_str()[self.pos..end].chars() {
                    if !c.is_xml_char() {
                        return Err(StreamError::NonXmlChar(c, self.gen_text_pos()));
                    }
                    if f(self, c) {
                        self.pos += c.len_utf8();
                    } else {
                        return Ok(());
                    }
                }
                break;
            }
        }
        Ok(())
    }

    // Returns Err on U+FFFE/U+FFFF at the current position, advances past 0xEF otherwise.
    #[inline(never)]
    fn check_ef_sequence(&mut self) -> Result<()> {
        let bytes = self.span.as_bytes();
        if self.pos + 2 < self.end && bytes[self.pos + 1] == 0xBF && bytes[self.pos + 2] >= 0xBE {
            let c = if bytes[self.pos + 2] == 0xBE {
                '\u{FFFE}'
            } else {
                '\u{FFFF}'
            };
            return Err(StreamError::NonXmlChar(c, self.gen_text_pos()));
        }
        self.pos += 1;
        Ok(())
    }

    /// Skips character data until `<`, validating XML chars and rejecting `]]>`.
    #[inline]
    pub(crate) fn skip_text_content(&mut self) -> Result<()> {
        let bytes = self.span.as_str().as_bytes();
        let end = self.end;
        while self.pos < end {
            let b = bytes[self.pos];
            if TEXT_OK[b as usize] {
                self.pos += 1;
                continue;
            }
            match b {
                b'<' => break,
                b']' => {
                    if self.pos + 2 < end
                        && bytes[self.pos + 1] == b']'
                        && bytes[self.pos + 2] == b'>'
                    {
                        return Err(StreamError::InvalidCharacterData);
                    }
                    self.pos += 1;
                }
                0xEF => self.check_ef_sequence()?,
                _ => return Err(StreamError::NonXmlChar(b as char, self.gen_text_pos())),
            }
        }
        Ok(())
    }

    /// Skips comment text, stopping right before the closing `-->`.
    ///
    /// A single pass detects `--` inside the text (`InvalidCommentData`),
    /// text ending with `-` (`InvalidCommentEnd`) and non-XML chars.
    /// If there is no `-->` terminator at all, everything is consumed and
    /// the caller's `skip_string(b"-->")` reports the error.
    #[inline]
    pub(crate) fn skip_comment_text(&mut self) -> Result<()> {
        let bytes = self.span.as_str().as_bytes();
        let end = self.end;
        let mut saw_double_dash = false;
        while self.pos < end {
            let b = bytes[self.pos];
            if COMMENT_OK[b as usize] {
                self.pos += 1;
                continue;
            }
            match b {
                b'-' => {
                    if self.pos + 1 < end && bytes[self.pos + 1] == b'-' {
                        // A run of dashes; find where it ends.
                        let p = self.pos;
                        let mut q = p + 2;
                        while q < end && bytes[q] == b'-' {
                            q += 1;
                        }
                        if q < end && bytes[q] == b'>' {
                            // "-->" found; the text ends two dashes before it,
                            // so longer runs leave dashes inside the text.
                            if saw_double_dash || q - p >= 4 {
                                return Err(StreamError::InvalidCommentData);
                            }
                            if q - p == 3 {
                                return Err(StreamError::InvalidCommentEnd);
                            }
                            self.pos = q - 2;
                            return Ok(());
                        }
                        if q >= end {
                            // Unterminated comment ending in dashes.
                            self.pos = q;
                            return Ok(());
                        }
                        // "--" inside the text. The error is deferred until
                        // the terminator is found, matching the old two-pass
                        // behavior for unterminated comments.
                        saw_double_dash = true;
                        self.pos = q;
                    } else {
                        self.pos += 1;
                    }
                }
                0xEF => self.check_ef_sequence()?,
                _ => return Err(StreamError::NonXmlChar(b as char, self.gen_text_pos())),
            }
        }
        Ok(())
    }

    /// Skips an attribute value until the closing quote, validating XML chars.
    ///
    /// Stops at `<` as well, which is not allowed inside attribute values.
    #[inline]
    pub(crate) fn skip_attr_value(&mut self, quote: u8) -> Result<()> {
        let bytes = self.span.as_str().as_bytes();
        let end = self.end;
        while self.pos < end {
            let b = bytes[self.pos];
            if b == quote || b == b'<' {
                break;
            }
            if TEXT_OK[b as usize] {
                self.pos += 1;
                continue;
            }
            match b {
                b']' => self.pos += 1,
                0xEF => self.check_ef_sequence()?,
                _ => return Err(StreamError::NonXmlChar(b as char, self.gen_text_pos())),
            }
        }
        Ok(())
    }

    #[inline]
    pub(crate) fn chars(&self) -> str::Chars<'a> {
        self.span.as_str()[self.pos..self.end].chars()
    }

    /// Slices data from `pos` to the current position.
    #[inline]
    pub fn slice_back(&self, pos: usize) -> StrSpan<'a> {
        self.span.slice_region(pos, self.pos)
    }

    /// Slices data from the current position to the end.
    #[inline]
    pub fn slice_tail(&self) -> StrSpan<'a> {
        self.span.slice_region(self.pos, self.end)
    }

    /// Skips whitespaces.
    ///
    /// Accepted values: `' ' \n \r \t`.
    #[inline]
    pub fn skip_spaces(&mut self) {
        let bytes = self.span.as_bytes();
        let end = self.end;
        while self.pos < end && bytes[self.pos].is_xml_space() {
            self.pos += 1;
        }
    }

    /// Checks if the stream is starts with a space.
    #[inline]
    pub fn starts_with_space(&self) -> bool {
        self.pos < self.end && self.span.as_bytes()[self.pos].is_xml_space()
    }

    /// Consumes whitespaces.
    ///
    /// Like [`skip_spaces()`], but checks that first char is actually a space.
    ///
    /// [`skip_spaces()`]: #method.skip_spaces
    ///
    /// # Errors
    ///
    /// - `InvalidSpace`
    pub fn consume_spaces(&mut self) -> Result<()> {
        if self.at_end() {
            return Err(StreamError::UnexpectedEndOfStream);
        }

        if !self.starts_with_space() {
            return Err(StreamError::InvalidSpace(
                self.curr_byte_unchecked(),
                self.gen_text_pos(),
            ));
        }

        self.skip_spaces();
        Ok(())
    }

    /// Consumes an XML character reference if there is one.
    ///
    /// On error will reset the position to the original.
    pub fn try_consume_reference(&mut self) -> Option<Reference<'a>> {
        let start = self.pos();

        // Consume reference on a substream.
        let mut s = *self;
        match s.consume_reference() {
            Ok(r) => {
                // If the current data is a reference than advance the current stream
                // by number of bytes read by substream.
                self.advance(s.pos() - start);
                Some(r)
            }
            Err(_) => None,
        }
    }

    /// Consumes an XML reference.
    ///
    /// Consumes according to: <https://www.w3.org/TR/xml/#NT-Reference>
    ///
    /// # Errors
    ///
    /// - `InvalidReference`
    pub fn consume_reference(&mut self) -> Result<Reference<'a>> {
        self._consume_reference()
            .map_err(|_| StreamError::InvalidReference)
    }

    #[inline(never)]
    fn _consume_reference(&mut self) -> Result<Reference<'a>> {
        if !self.try_consume_byte(b'&') {
            return Err(StreamError::InvalidReference);
        }

        let reference = if self.try_consume_byte(b'#') {
            let (value, radix) = if self.try_consume_byte(b'x') {
                let value = self.consume_bytes(|_, c| c.is_xml_hex_digit()).as_str();
                (value, 16)
            } else {
                let value = self.consume_bytes(|_, c| c.is_xml_digit()).as_str();
                (value, 10)
            };

            let n = u32::from_str_radix(value, radix).map_err(|_| StreamError::InvalidReference)?;

            let c = char::from_u32(n).unwrap_or('\u{FFFD}');
            if !c.is_xml_char() {
                return Err(StreamError::InvalidReference);
            }

            Reference::Char(c)
        } else {
            let name = self.consume_name()?;
            match name.as_str() {
                "quot" => Reference::Char('"'),
                "amp" => Reference::Char('&'),
                "apos" => Reference::Char('\''),
                "lt" => Reference::Char('<'),
                "gt" => Reference::Char('>'),
                _ => Reference::Entity(name.as_str()),
            }
        };

        self.consume_byte(b';')?;

        Ok(reference)
    }

    /// Consumes an XML name and returns it.
    ///
    /// Consumes according to: <https://www.w3.org/TR/xml/#NT-Name>
    ///
    /// # Errors
    ///
    /// - `InvalidName` - if name is empty or starts with an invalid char
    /// - `UnexpectedEndOfStream`
    #[inline]
    pub fn consume_name(&mut self) -> Result<StrSpan<'a>> {
        let start = self.pos();
        self.skip_name()?;

        let name = self.slice_back(start);
        if name.is_empty() {
            return Err(StreamError::InvalidName);
        }

        Ok(name)
    }

    /// Skips an XML name.
    ///
    /// The same as `consume_name()`, but does not return a consumed name.
    ///
    /// # Errors
    ///
    /// - `InvalidName` - if name is empty or starts with an invalid char
    pub fn skip_name(&mut self) -> Result<()> {
        let mut iter = self.chars();
        if let Some(c) = iter.next() {
            if c.is_xml_name_start() {
                self.advance(c.len_utf8());
            } else {
                return Err(StreamError::InvalidName);
            }
        }

        for c in iter {
            if c.is_xml_name() {
                self.advance(c.len_utf8());
            } else {
                break;
            }
        }

        Ok(())
    }

    /// Consumes a qualified XML name and returns it.
    ///
    /// Consumes according to: <https://www.w3.org/TR/xml-names/#ns-qualnames>
    ///
    /// # Errors
    ///
    /// - `InvalidName` - if name is empty or starts with an invalid char
    #[inline]
    pub fn consume_qname(&mut self) -> Result<(StrSpan<'a>, StrSpan<'a>)> {
        let start = self.pos;

        let mut splitter = None;
        let bytes = self.span.as_bytes();
        let end = self.end;

        loop {
            // Fast scan over ASCII name bytes; specials handled below.
            while self.pos < end && NAME_CLASS[bytes[self.pos] as usize] {
                self.pos += 1;
            }

            if self.pos >= end {
                break;
            }

            let b = bytes[self.pos];
            if b == b':' {
                if splitter.is_none() {
                    splitter = Some(self.pos);
                    self.pos += 1;
                } else {
                    return Err(StreamError::InvalidName);
                }
            } else if b >= 128 {
                match self.span.as_str()[self.pos..].chars().next() {
                    Some(c) if c.is_xml_name() => {
                        self.pos += c.len_utf8();
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

        let (prefix, local) = if let Some(splitter) = splitter {
            let prefix = self.span().slice_region(start, splitter);
            let local = self.slice_back(splitter + 1);
            (prefix, local)
        } else {
            let local = self.slice_back(start);
            ("".into(), local)
        };

        // Prefix must start with a `NameStartChar`.
        if let Some(c) = prefix.as_str().chars().nth(0) {
            if !c.is_xml_name_start() {
                return Err(StreamError::InvalidName);
            }
        }

        // Local name must start with a `NameStartChar`.
        if let Some(c) = local.as_str().chars().nth(0) {
            if !c.is_xml_name_start() {
                return Err(StreamError::InvalidName);
            }
        } else {
            // If empty - error.
            return Err(StreamError::InvalidName);
        }

        Ok((prefix, local))
    }

    /// Consumes `=`.
    ///
    /// Consumes according to: <https://www.w3.org/TR/xml/#NT-Eq>
    ///
    /// # Errors
    ///
    /// - `InvalidChar`
    /// - `UnexpectedEndOfStream`
    pub fn consume_eq(&mut self) -> Result<()> {
        self.skip_spaces();
        self.consume_byte(b'=')?;
        self.skip_spaces();

        Ok(())
    }

    /// Consumes quote.
    ///
    /// Consumes `'` or `"` and returns it.
    ///
    /// # Errors
    ///
    /// - `InvalidQuote`
    /// - `UnexpectedEndOfStream`
    pub fn consume_quote(&mut self) -> Result<u8> {
        let c = self.curr_byte()?;
        if c == b'\'' || c == b'"' {
            self.advance(1);
            Ok(c)
        } else {
            Err(StreamError::InvalidQuote(c, self.gen_text_pos()))
        }
    }

    /// Calculates a current absolute position.
    ///
    /// This operation is very expensive. Use only for errors.
    #[inline(never)]
    pub fn gen_text_pos(&self) -> TextPos {
        let text = self.span.as_str();
        let end = self.pos;

        let row = Self::calc_curr_row(text, end);
        let col = Self::calc_curr_col(text, end);
        TextPos::new(row, col)
    }

    /// Calculates an absolute position at `pos`.
    ///
    /// This operation is very expensive. Use only for errors.
    ///
    /// # Examples
    ///
    /// ```
    /// let s = xmltok::Stream::from("text");
    ///
    /// assert_eq!(s.gen_text_pos_from(2), xmltok::TextPos::new(1, 3));
    /// assert_eq!(s.gen_text_pos_from(9999), xmltok::TextPos::new(1, 5));
    /// ```
    #[inline(never)]
    pub fn gen_text_pos_from(&self, pos: usize) -> TextPos {
        let mut s = *self;
        s.pos = cmp::min(pos, s.span.as_str().len());
        s.gen_text_pos()
    }

    fn calc_curr_row(text: &str, end: usize) -> u32 {
        let mut row = 1;
        for c in &text.as_bytes()[..end] {
            if *c == b'\n' {
                row += 1;
            }
        }

        row
    }

    fn calc_curr_col(text: &str, end: usize) -> u32 {
        let mut col = 1;
        for c in text[..end].chars().rev() {
            if c == '\n' {
                break;
            } else {
                col += 1;
            }
        }

        col
    }
}
