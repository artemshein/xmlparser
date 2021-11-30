/*!
*xmlparser* is a low-level, pull-based, zero-allocation
[XML 1.0](https://www.w3.org/TR/xml/) parser.

## Example

```rust
for token in xmlparser::Tokenizer::from("<tagname name='value'/>") {
    println!("{:?}", token);
}
```

## Why a new library?

This library is basically a low-level XML tokenizer that preserves the positions of the tokens
and is not intended to be used directly.
If you are looking for a higher level solution, check out
[roxmltree](https://github.com/RazrFalcon/roxmltree).

## Benefits

- All tokens contain `StrSpan` structs which represent the position of the substring
  in the original document.
- Good error processing. All error types contain the position (line:column) where it occurred.
- No heap allocations.
- No dependencies.
- Tiny. ~1400 LOC and ~30KiB in the release build according to `cargo-bloat`.
- Supports `no_std` builds. To use without the standard library, disable the default features.

## Limitations

- Currently, only ENTITY objects are parsed from the DOCTYPE. All others are ignored.
- No tree structure validation. So an XML like `<root><child></root></child>`
  or a string without root element
  will be parsed without errors. You should check for this manually.
  On the other hand `<a/><a/>` will lead to an error.
- Duplicated attributes is not an error. So XML like `<item a="v1" a="v2"/>`
  will be parsed without errors. You should check for this manually.
- UTF-8 only.

## Safety

- The library must not panic. Any panic is considered a critical bug
  and should be reported.
- The library forbids unsafe code.
*/

#![no_std]
#![doc(html_root_url = "https://docs.rs/xmlparser/0.13.4")]
#![forbid(unsafe_code)]
#![warn(missing_docs)]
#![allow(ellipsis_inclusive_range_patterns)]

#[cfg(feature = "std")]
#[macro_use]
extern crate std;

macro_rules! matches {
    ($expression:expr, $($pattern:tt)+) => {
        match $expression {
            $($pattern)+ => true,
            _ => false
        }
    }
}

mod error;
mod stream;
mod strspan;
mod xmlchar;

pub use crate::error::*;
pub use crate::stream::*;
pub use crate::strspan::*;
pub use crate::xmlchar::*;

/// An XML token.
#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    /// Declaration token.
    ///
    /// ```text
    /// <?xml version='1.0' encoding='UTF-8' standalone='yes'?>
    ///                ---                                      - version
    ///                               -----                     - encoding?
    ///                                                  ---    - standalone?
    /// ------------------------------------------------------- - span
    /// ```
    Declaration {
        start: usize,
        version: SmallDetachedStrSpan,
        encoding: SmallDetachedStrSpan,
        standalone: Option<bool>,
        end: u16,
    },

    /// Processing instruction token.
    ///
    /// ```text
    /// <?target content?>
    ///   ------           - target
    ///          -------   - content?
    /// ------------------ - span
    /// ```
    ProcessingInstruction {
        start: usize,
        end: u16,
        target: SmallDetachedStrSpan,
        content: SmallDetachedStrSpan,
    },

    /// Comment token.
    ///
    /// ```text
    /// <!-- text -->
    ///     ------    - text
    /// ------------- - span
    /// ```
    Comment {
        start: usize,
        end: u32,
        text: DetachedStrSpan,
    },

    /// DOCTYPE start token.
    ///
    /// ```text
    /// <!DOCTYPE greeting [
    ///           --------                      - name
    /// --------------------------------------- - span
    /// ```
    DtdStartNoExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
    },

    /// DOCTYPE start token.
    ///
    /// ```text
    /// <!DOCTYPE greeting SYSTEM "hello.dtd" [
    ///           --------                      - name
    ///                    ------------------   - external_id
    /// --------------------------------------- - span
    /// ```
    DtdStartSystemExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        external_id: SmallDetachedStrSpan,
    },

    /// DOCTYPE start token.
    ///
    /// ```text
    /// <!DOCTYPE greeting PUBLIC "hello.dtd" [
    ///           --------                      - name
    ///                    ------------------   - external_id
    /// --------------------------------------- - span
    /// ```
    DtdStartPublicExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        public1: SmallDetachedStrSpan,
        public2: SmallDetachedStrSpan,
    },

    /// Empty DOCTYPE token.
    ///
    /// ```text
    /// <!DOCTYPE greeting SYSTEM "hello.dtd">
    ///           --------                     - name
    ///                    ------------------  - external_id?
    /// -------------------------------------- - span
    /// ```
    EmptyDtdNoExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
    },

    /// Empty DOCTYPE token.
    ///
    /// ```text
    /// <!DOCTYPE greeting SYSTEM "hello.dtd">
    ///           --------                     - name
    ///                    ------------------  - external_id?
    /// -------------------------------------- - span
    /// ```
    EmptyDtdSystemExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        external_id: SmallDetachedStrSpan,
    },

    /// Empty DOCTYPE token.
    ///
    /// ```text
    /// <!DOCTYPE greeting PUBLIC "hello.dtd">
    ///           --------                     - name
    ///                    ------------------  - external_id?
    /// -------------------------------------- - span
    /// ```
    EmptyDtdPublicExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        public1: SmallDetachedStrSpan,
        public2: SmallDetachedStrSpan,
    },

    /// ENTITY token.
    ///
    /// Can appear only inside the DTD.
    ///
    /// ```text
    /// <!ENTITY ns_extend "http://test.com">
    ///          ---------                    - name
    ///                     ---------------   - definition
    /// ------------------------------------- - span
    /// ```
    EntityDeclarationEntityValue {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        entity_value: SmallDetachedStrSpan,
    },

    /// ENTITY token.
    ///
    /// Can appear only inside the DTD.
    ///
    /// ```text
    /// <!ENTITY ns_extend "http://test.com">
    ///          ---------                    - name
    ///                     ---------------   - definition
    /// ------------------------------------- - span
    /// ```
    EntityDeclarationSystemExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        external_id: SmallDetachedStrSpan,
    },

    /// ENTITY token.
    ///
    /// Can appear only inside the DTD.
    ///
    /// ```text
    /// <!ENTITY ns_extend "http://test.com">
    ///          ---------                    - name
    ///                     ---------------   - definition
    /// ------------------------------------- - span
    /// ```
    EntityDeclarationPublicExternalId {
        start: usize,
        end: u16,
        name: SmallDetachedStrSpan,
        public1: SmallDetachedStrSpan,
        public2: SmallDetachedStrSpan,
    },

    /// DOCTYPE end token.
    ///
    /// ```text
    /// <!DOCTYPE svg [
    ///    ...
    /// ]>
    /// -- - span
    /// ```
    DtdEnd { start: usize, end: u16 },

    /// Element start token.
    ///
    /// ```text
    /// <ns:elem attr="value"/>
    ///  --                     - prefix
    ///     ----                - local
    /// --------                - span
    /// ```
    ElementStart {
        start: usize,
        end: u16,
        prefix: SmallDetachedStrSpan,
        local: SmallDetachedStrSpan,
    },

    /// Attribute token.
    ///
    /// ```text
    /// <elem ns:attr="value"/>
    ///       --              - prefix
    ///          ----         - local
    ///                -----  - value
    ///       --------------- - span
    /// ```
    Attribute {
        start: usize,
        end: u16,
        prefix: SmallDetachedStrSpan,
        local: SmallDetachedStrSpan,
        value: SmallDetachedStrSpan,
    },

    /// Element end token.
    ///
    /// ```text
    /// <ns:elem>text</ns:elem>
    ///                         - ElementEnd::Open
    ///         -               - span
    /// ```
    ///
    /// ```text
    /// <ns:elem>text</ns:elem>
    ///                -- ----  - ElementEnd::Close(prefix, local)
    ///              ---------- - span
    /// ```
    ///
    /// ```text
    /// <ns:elem/>
    ///                         - ElementEnd::Empty
    ///         --              - span
    /// ```
    ElementEnd {
        start: usize,
        end: u16,
        el_end: ElementEnd,
    },

    /// Text token.
    ///
    /// Contains text between elements including whitespaces.
    /// Basically everything between `>` and `<`.
    /// Except `]]>`, which is not allowed and will lead to an error.
    ///
    /// ```text
    /// <p> text </p>
    ///    ------     - text
    /// ```
    ///
    /// The token span is equal to the `text`.
    Text {
        start: usize,
        end: u32,
        text: DetachedStrSpan,
    },

    /// CDATA token.
    ///
    /// ```text
    /// <p><![CDATA[text]]></p>
    ///             ----        - text
    ///    ----------------     - span
    /// ```
    Cdata {
        start: usize,
        end: u32,
        text: DetachedStrSpan,
    },
}

/// `ElementEnd` token.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ElementEnd {
    /// Indicates `>`
    Open,
    /// Indicates `</name>`
    Close(SmallDetachedStrSpan, SmallDetachedStrSpan),
    /// Indicates `/>`
    Empty,
}

/// Representation of the [ExternalID](https://www.w3.org/TR/xml/#NT-ExternalID) value.
#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ExternalId {
    System(SmallDetachedStrSpan),
    Public(SmallDetachedStrSpan, SmallDetachedStrSpan),
}

/// Representation of the [EntityDef](https://www.w3.org/TR/xml/#NT-EntityDef) value.
#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum EntityDefinition {
    EntityValue(SmallDetachedStrSpan),
    ExternalId(ExternalId),
}

type Result<T> = core::result::Result<T, Error>;
type StreamResult<T> = core::result::Result<T, StreamError>;

#[derive(Clone, Copy, PartialEq)]
enum State {
    Declaration,
    AfterDeclaration,
    Dtd,
    AfterDtd,
    Elements,
    Attributes,
    AfterElements,
    End,
}

/// Tokenizer for the XML structure.
pub struct Tokenizer<'a> {
    stream: Stream<'a>,
    state: State,
    depth: usize,
    fragment_parsing: bool,
}

impl<'a> From<&'a str> for Tokenizer<'a> {
    #[inline]
    fn from(text: &'a str) -> Self {
        let mut stream = Stream::from(text);

        // Skip UTF-8 BOM.
        if stream.starts_with(&[0xEF, 0xBB, 0xBF]) {
            stream.advance(3);
        }

        Tokenizer {
            stream,
            state: State::Declaration,
            depth: 0,
            fragment_parsing: false,
        }
    }
}

macro_rules! map_err_at {
    ($fun:expr, $stream:expr, $err:ident) => {{
        let start = $stream.pos();
        $fun.map_err(|e| Error::$err(e, $stream.gen_text_pos_from(start)))
    }};
}

impl<'a> Tokenizer<'a> {
    /// Enables document fragment parsing.
    ///
    /// By default, `xmlparser` will check for DTD, root element, etc.
    /// But if we have to parse an XML fragment, it will lead to an error.
    /// This method switches the parser to the root element content parsing mode,
    /// so it will treat any data as a content of the root element.
    pub fn from_fragment(full_text: &'a str, fragment: core::ops::Range<usize>) -> Self {
        Tokenizer {
            stream: Stream::from_substr(full_text, fragment),
            state: State::Elements,
            depth: 0,
            fragment_parsing: true,
        }
    }

    fn parse_next_impl(&mut self) -> Option<Result<Token>> {
        let s = &mut self.stream;

        if s.at_end() {
            return None;
        }

        let start = s.pos();

        match self.state {
            State::Declaration => {
                self.state = State::AfterDeclaration;
                if s.starts_with(b"<?xml ") {
                    Some(Self::parse_declaration(s))
                } else {
                    self.parse_next_impl()
                }
            }
            State::AfterDeclaration => {
                if s.starts_with(b"<!DOCTYPE") {
                    let t = Self::parse_doctype(s);
                    match t {
                        Ok(
                            Token::DtdStartNoExternalId { .. }
                            | Token::DtdStartSystemExternalId { .. }
                            | Token::DtdStartPublicExternalId { .. },
                        ) => self.state = State::Dtd,
                        Ok(
                            Token::EmptyDtdNoExternalId { .. }
                            | Token::EmptyDtdSystemExternalId { .. }
                            | Token::EmptyDtdPublicExternalId { .. },
                        ) => self.state = State::AfterDtd,
                        _ => {}
                    }

                    Some(t)
                } else if s.starts_with(b"<!--") {
                    Some(Self::parse_comment(s))
                } else if s.starts_with(b"<?") {
                    if s.starts_with(b"<?xml ") {
                        Some(Err(Error::UnknownToken(s.gen_text_pos())))
                    } else {
                        Some(Self::parse_pi(s))
                    }
                } else if s.starts_with_space() {
                    s.skip_spaces();
                    self.parse_next_impl()
                } else {
                    self.state = State::AfterDtd;
                    self.parse_next_impl()
                }
            }
            State::Dtd => {
                if s.starts_with(b"<!ENTITY") {
                    Some(Self::parse_entity_decl(s))
                } else if s.starts_with(b"<!--") {
                    Some(Self::parse_comment(s))
                } else if s.starts_with(b"<?") {
                    if s.starts_with(b"<?xml ") {
                        Some(Err(Error::UnknownToken(s.gen_text_pos())))
                    } else {
                        Some(Self::parse_pi(s))
                    }
                } else if s.starts_with(b"]") {
                    // DTD ends with ']' S? '>', therefore we have to skip possible spaces.
                    s.advance(1);
                    s.skip_spaces();
                    match s.curr_byte() {
                        Ok(b'>') => {
                            self.state = State::AfterDtd;
                            s.advance(1);
                            Some(Ok(Token::DtdEnd {
                                start,
                                end: (s.pos() - start) as u16,
                            }))
                        }
                        Ok(c) => {
                            let e = StreamError::InvalidChar(c, b'>', s.gen_text_pos());
                            Some(Err(Error::InvalidDoctype(e, s.gen_text_pos_from(start))))
                        }
                        Err(_) => {
                            let e = StreamError::UnexpectedEndOfStream;
                            Some(Err(Error::InvalidDoctype(e, s.gen_text_pos_from(start))))
                        }
                    }
                } else if s.starts_with_space() {
                    s.skip_spaces();
                    self.parse_next_impl()
                } else if s.starts_with(b"<!ELEMENT")
                    || s.starts_with(b"<!ATTLIST")
                    || s.starts_with(b"<!NOTATION")
                {
                    if Self::consume_decl(s).is_err() {
                        let pos = s.gen_text_pos_from(start);
                        Some(Err(Error::UnknownToken(pos)))
                    } else {
                        self.parse_next_impl()
                    }
                } else {
                    Some(Err(Error::UnknownToken(s.gen_text_pos())))
                }
            }
            State::AfterDtd => {
                if s.starts_with(b"<!--") {
                    Some(Self::parse_comment(s))
                } else if s.starts_with(b"<?") {
                    if s.starts_with(b"<?xml ") {
                        Some(Err(Error::UnknownToken(s.gen_text_pos())))
                    } else {
                        Some(Self::parse_pi(s))
                    }
                } else if s.starts_with(b"<!") {
                    Some(Err(Error::UnknownToken(s.gen_text_pos())))
                } else if s.starts_with(b"<") {
                    self.state = State::Attributes;
                    Some(Self::parse_element_start(s))
                } else if s.starts_with_space() {
                    s.skip_spaces();
                    self.parse_next_impl()
                } else {
                    Some(Err(Error::UnknownToken(s.gen_text_pos())))
                }
            }
            State::Elements => {
                // Use `match` only here, because only this section is performance-critical.
                match s.curr_byte() {
                    Ok(b'<') => match s.next_byte() {
                        Ok(b'!') => {
                            if s.starts_with(b"<!--") {
                                Some(Self::parse_comment(s))
                            } else if s.starts_with(b"<![CDATA[") {
                                Some(Self::parse_cdata(s))
                            } else {
                                Some(Err(Error::UnknownToken(s.gen_text_pos())))
                            }
                        }
                        Ok(b'?') => {
                            if !s.starts_with(b"<?xml ") {
                                Some(Self::parse_pi(s))
                            } else {
                                Some(Err(Error::UnknownToken(s.gen_text_pos())))
                            }
                        }
                        Ok(b'/') => {
                            if self.depth > 0 {
                                self.depth -= 1;
                            }

                            if self.depth == 0 && !self.fragment_parsing {
                                self.state = State::AfterElements;
                            } else {
                                self.state = State::Elements;
                            }

                            Some(Self::parse_close_element(s))
                        }
                        Ok(_) => {
                            self.state = State::Attributes;
                            Some(Self::parse_element_start(s))
                        }
                        Err(_) => {
                            return Some(Err(Error::UnknownToken(s.gen_text_pos())));
                        }
                    },
                    Ok(_) => Some(Self::parse_text(s)),
                    Err(_) => Some(Err(Error::UnknownToken(s.gen_text_pos()))),
                }
            }
            State::Attributes => {
                let t = Self::parse_attribute(s);

                if let Ok(Token::ElementEnd { el_end, .. }) = t {
                    if el_end == ElementEnd::Open {
                        self.depth += 1;
                    }

                    if self.depth == 0 && !self.fragment_parsing {
                        self.state = State::AfterElements;
                    } else {
                        self.state = State::Elements;
                    }
                }

                Some(t.map_err(|e| Error::InvalidAttribute(e, s.gen_text_pos_from(start))))
            }
            State::AfterElements => {
                if s.starts_with(b"<!--") {
                    Some(Self::parse_comment(s))
                } else if s.starts_with(b"<?") {
                    if s.starts_with(b"<?xml ") {
                        Some(Err(Error::UnknownToken(s.gen_text_pos())))
                    } else {
                        Some(Self::parse_pi(s))
                    }
                } else if s.starts_with_space() {
                    s.skip_spaces();
                    self.parse_next_impl()
                } else {
                    Some(Err(Error::UnknownToken(s.gen_text_pos())))
                }
            }
            State::End => None,
        }
    }

    fn parse_declaration(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_declaration_impl(s), s, InvalidDeclaration)
    }

    // XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    fn parse_declaration_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        fn consume_spaces(s: &mut Stream) -> StreamResult<()> {
            if s.starts_with_space() {
                s.skip_spaces();
            } else if !s.starts_with(b"?>") && !s.at_end() {
                return Err(StreamError::InvalidSpace(
                    s.curr_byte_unchecked(),
                    s.gen_text_pos(),
                ));
            }

            Ok(())
        }

        let start = s.pos();
        s.advance(6);

        let version = Self::parse_version_info(s)?;
        consume_spaces(s)?;

        let encoding = Self::parse_encoding_decl(s)?;
        if encoding.is_some() {
            consume_spaces(s)?;
        }

        let standalone = Self::parse_standalone(s)?;

        s.skip_spaces();
        s.skip_string(b"?>")?;

        Ok(Token::Declaration {
            start,
            version: version.detach_small(start),
            encoding: encoding
                .map(|e| e.detach_small(start))
                .unwrap_or_else(SmallDetachedStrSpan::empty),
            standalone,
            end: (s.pos() - start) as u16,
        })
    }

    // VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
    // VersionNum  ::= '1.' [0-9]+
    fn parse_version_info(s: &mut Stream<'a>) -> StreamResult<StrSpan<'a>> {
        s.skip_spaces();
        s.skip_string(b"version")?;
        s.consume_eq()?;
        let quote = s.consume_quote()?;

        let start = s.pos();
        s.skip_string(b"1.")?;
        s.skip_bytes(|_, c| c.is_xml_digit());
        let ver = s.slice_back(start);

        s.consume_byte(quote)?;

        Ok(ver)
    }

    // EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
    // EncName      ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
    fn parse_encoding_decl(s: &mut Stream<'a>) -> StreamResult<Option<StrSpan<'a>>> {
        if !s.starts_with(b"encoding") {
            return Ok(None);
        }

        s.advance(8);
        s.consume_eq()?;
        let quote = s.consume_quote()?;
        // [A-Za-z] ([A-Za-z0-9._] | '-')*
        // TODO: check that first byte is [A-Za-z]
        let name = s.consume_bytes(|_, c| {
            c.is_xml_letter() || c.is_xml_digit() || c == b'.' || c == b'-' || c == b'_'
        });
        s.consume_byte(quote)?;

        Ok(Some(name))
    }

    // SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    fn parse_standalone(s: &mut Stream<'a>) -> StreamResult<Option<bool>> {
        if !s.starts_with(b"standalone") {
            return Ok(None);
        }

        s.advance(10);
        s.consume_eq()?;
        let quote = s.consume_quote()?;

        let start = s.pos();
        let value = s.consume_name()?.as_str();

        let flag = match value {
            "yes" => true,
            "no" => false,
            _ => {
                let pos = s.gen_text_pos_from(start);

                return Err(StreamError::InvalidString("yes', 'no", pos));
            }
        };

        s.consume_byte(quote)?;

        Ok(Some(flag))
    }

    fn parse_comment(s: &mut Stream<'a>) -> Result<Token> {
        let start = s.pos();
        Self::parse_comment_impl(s)
            .map_err(|e| Error::InvalidComment(e, s.gen_text_pos_from(start)))
    }

    // '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
    fn parse_comment_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(4);
        let text = s.consume_chars(|s, c| !(c == '-' && s.starts_with(b"-->")))?;
        s.skip_string(b"-->")?;

        if text.as_str().contains("--") {
            return Err(StreamError::InvalidCommentData);
        }

        if text.as_str().ends_with('-') {
            return Err(StreamError::InvalidCommentEnd);
        }

        Ok(Token::Comment {
            start,
            end: (s.pos() - start) as u32,
            text: text.detach(start),
        })
    }

    fn parse_pi(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_pi_impl(s), s, InvalidPI)
    }

    // PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
    // PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
    fn parse_pi_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(2);
        let target = s.consume_name()?;
        s.skip_spaces();
        let content = s.consume_chars(|s, c| !(c == '?' && s.starts_with(b"?>")))?;
        let content = if !content.is_empty() {
            Some(content)
        } else {
            None
        };

        s.skip_string(b"?>")?;

        Ok(Token::ProcessingInstruction {
            start,
            end: (s.pos() - start) as u16,
            target: target.detach_small(start),
            content: content
                .map(|c| c.detach_small(start))
                .unwrap_or_else(SmallDetachedStrSpan::empty),
        })
    }

    fn parse_doctype(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_doctype_impl(s), s, InvalidDoctype)
    }

    // doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
    fn parse_doctype_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(9);

        s.consume_spaces()?;
        let name = s.consume_name()?;
        s.skip_spaces();

        let external_id = Self::parse_external_id(s, start)?;
        s.skip_spaces();

        let c = s.curr_byte()?;
        if c != b'[' && c != b'>' {
            static EXPECTED: &[u8] = &[b'[', b'>'];
            return Err(StreamError::InvalidCharMultiple(
                c,
                EXPECTED,
                s.gen_text_pos(),
            ));
        }

        s.advance(1);

        if c == b'[' {
            Ok(match external_id {
                None => Token::DtdStartNoExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                },
                Some(ExternalId::System(ext_id)) => Token::DtdStartSystemExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                    external_id: ext_id,
                },
                Some(ExternalId::Public(p1, p2)) => Token::DtdStartPublicExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                    public1: p1,
                    public2: p2,
                },
            })
        } else {
            Ok(match external_id {
                None => Token::EmptyDtdNoExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                },
                Some(ExternalId::System(ext_id)) => Token::EmptyDtdSystemExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                    external_id: ext_id,
                },
                Some(ExternalId::Public(p1, p2)) => Token::EmptyDtdPublicExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                    public1: p1,
                    public2: p2,
                },
            })
        }
    }

    // ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    fn parse_external_id(s: &mut Stream<'a>, start: usize) -> StreamResult<Option<ExternalId>> {
        let v = if s.starts_with(b"SYSTEM") || s.starts_with(b"PUBLIC") {
            let local_start = s.pos();
            s.advance(6);
            let id = s.slice_back(local_start);

            s.consume_spaces()?;
            let quote = s.consume_quote()?;
            let literal1 = s.consume_bytes(|_, c| c != quote);
            s.consume_byte(quote)?;

            let v = if id.as_str() == "SYSTEM" {
                ExternalId::System(literal1.detach_small(start))
            } else {
                s.consume_spaces()?;
                let quote = s.consume_quote()?;
                let literal2 = s.consume_bytes(|_, c| c != quote);
                s.consume_byte(quote)?;

                ExternalId::Public(literal1.detach_small(start), literal2.detach_small(start))
            };

            Some(v)
        } else {
            None
        };

        Ok(v)
    }

    fn parse_entity_decl(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_entity_decl_impl(s), s, InvalidEntity)
    }

    // EntityDecl  ::= GEDecl | PEDecl
    // GEDecl      ::= '<!ENTITY' S Name S EntityDef S? '>'
    // PEDecl      ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
    fn parse_entity_decl_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(8);

        s.consume_spaces()?;

        let is_ge = if s.try_consume_byte(b'%') {
            s.consume_spaces()?;
            false
        } else {
            true
        };

        let name = s.consume_name()?;
        s.consume_spaces()?;
        let definition = Self::parse_entity_def(s, is_ge, start)?;
        s.skip_spaces();
        s.consume_byte(b'>')?;

        Ok(match definition {
            EntityDefinition::EntityValue(entity_value) => Token::EntityDeclarationEntityValue {
                start,
                end: (s.pos() - start) as u16,
                name: name.detach_small(start),
                entity_value,
            },
            EntityDefinition::ExternalId(ExternalId::System(ext_id)) => {
                Token::EntityDeclarationSystemExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                    external_id: ext_id,
                }
            }
            EntityDefinition::ExternalId(ExternalId::Public(p1, p2)) => {
                Token::EntityDeclarationPublicExternalId {
                    start,
                    end: (s.pos() - start) as u16,
                    name: name.detach_small(start),
                    public1: p1,
                    public2: p2,
                }
            }
        })
    }

    // EntityDef   ::= EntityValue | (ExternalID NDataDecl?)
    // PEDef       ::= EntityValue | ExternalID
    // EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&']
    //                             | PEReference | Reference)* "'"
    // ExternalID  ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
    // NDataDecl   ::= S 'NDATA' S Name
    fn parse_entity_def(
        s: &mut Stream<'a>,
        is_ge: bool,
        start: usize,
    ) -> StreamResult<EntityDefinition> {
        let c = s.curr_byte()?;
        match c {
            b'"' | b'\'' => {
                let quote = s.consume_quote()?;
                let value = s.consume_bytes(|_, c| c != quote);
                s.consume_byte(quote)?;

                Ok(EntityDefinition::EntityValue(value.detach_small(start)))
            }
            b'S' | b'P' => {
                if let Some(id) = Self::parse_external_id(s, start)? {
                    if is_ge {
                        s.skip_spaces();
                        if s.starts_with(b"NDATA") {
                            s.advance(5);
                            s.consume_spaces()?;
                            s.skip_name()?;
                            // TODO: NDataDecl is not supported
                        }
                    }

                    Ok(EntityDefinition::ExternalId(id))
                } else {
                    Err(StreamError::InvalidExternalID)
                }
            }
            _ => {
                static EXPECTED: &[u8] = &[b'"', b'\'', b'S', b'P'];
                let pos = s.gen_text_pos();
                Err(StreamError::InvalidCharMultiple(c, EXPECTED, pos))
            }
        }
    }

    fn consume_decl(s: &mut Stream) -> StreamResult<()> {
        s.skip_bytes(|_, c| c != b'>');
        s.consume_byte(b'>')?;
        Ok(())
    }

    fn parse_cdata(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_cdata_impl(s), s, InvalidCdata)
    }

    // CDSect  ::= CDStart CData CDEnd
    // CDStart ::= '<![CDATA['
    // CData   ::= (Char* - (Char* ']]>' Char*))
    // CDEnd   ::= ']]>'
    fn parse_cdata_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(9);
        let text = s.consume_chars(|s, c| !(c == ']' && s.starts_with(b"]]>")))?;
        s.skip_string(b"]]>")?;
        Ok(Token::Cdata {
            start,
            end: (s.pos() - start) as u32,
            text: text.detach(start),
        })
    }

    fn parse_element_start(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_element_start_impl(s), s, InvalidElement)
    }

    // '<' Name (S Attribute)* S? '>'
    fn parse_element_start_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(1);
        let (prefix, local) = s.consume_qname()?;
        Ok(Token::ElementStart {
            start,
            end: (s.pos() - start) as u16,
            prefix: prefix.detach_small(start),
            local: local.detach_small(start),
        })
    }

    fn parse_close_element(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_close_element_impl(s), s, InvalidElement)
    }

    // '</' Name S? '>'
    fn parse_close_element_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        s.advance(2);

        let (prefix, tag_name) = s.consume_qname()?;
        s.skip_spaces();
        s.consume_byte(b'>')?;
        Ok(Token::ElementEnd {
            start,
            end: (s.pos() - start) as u16,
            el_end: ElementEnd::Close(prefix.detach_small(0), tag_name.detach_small(0)),
        })
    }

    // Name Eq AttValue
    fn parse_attribute(s: &mut Stream<'a>) -> StreamResult<Token> {
        let attr_start = s.pos();
        let has_space = s.starts_with_space();
        s.skip_spaces();

        if let Ok(c) = s.curr_byte() {
            let start = s.pos();

            match c {
                b'/' => {
                    s.advance(1);
                    s.consume_byte(b'>')?;
                    return Ok(Token::ElementEnd {
                        start,
                        end: (s.pos() - start) as u16,
                        el_end: ElementEnd::Empty,
                    });
                }
                b'>' => {
                    s.advance(1);
                    return Ok(Token::ElementEnd {
                        start,
                        end: (s.pos() - start) as u16,
                        el_end: ElementEnd::Open,
                    });
                }
                _ => {}
            }
        }

        if !has_space {
            return if !s.at_end() {
                Err(StreamError::InvalidSpace(
                    s.curr_byte_unchecked(),
                    s.gen_text_pos_from(attr_start),
                ))
            } else {
                Err(StreamError::UnexpectedEndOfStream)
            };
        }

        let start = s.pos();

        let (prefix, local) = s.consume_qname()?;
        s.consume_eq()?;
        let quote = s.consume_quote()?;
        let quote_c = quote as char;
        // The attribute value must not contain the < character.
        let value = s.consume_chars(|_, c| c != quote_c && c != '<')?;
        s.consume_byte(quote)?;

        Ok(Token::Attribute {
            start,
            end: (s.pos() - start) as u16,
            prefix: prefix.detach_small(start),
            local: local.detach_small(start),
            value: value.detach_small(start),
        })
    }

    fn parse_text(s: &mut Stream<'a>) -> Result<Token> {
        map_err_at!(Self::parse_text_impl(s), s, InvalidCharData)
    }

    fn parse_text_impl(s: &mut Stream<'a>) -> StreamResult<Token> {
        let start = s.pos();
        let text = s.consume_chars(|_, c| c != '<')?;

        // According to the spec, `]]>` must not appear inside a Text node.
        // https://www.w3.org/TR/xml/#syntax
        //
        // Search for `>` first, since it's a bit faster than looking for `]]>`.
        if text.as_str().contains('>') {
            if text.as_str().contains("]]>") {
                return Err(StreamError::InvalidCharacterData);
            }
        }

        Ok(Token::Text {
            start,
            end: (s.pos() - start) as u32,
            text: text.detach(start),
        })
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.stream.at_end() || self.state == State::End {
            return None;
        }

        let t = self.parse_next_impl();

        if let Some(Err(_)) = t {
            self.stream.jump_to_end();
            self.state = State::End;
        }

        t
    }
}
