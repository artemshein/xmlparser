## xmlparser (fork)

This is a fork of [RazrFalcon/xmlparser](https://github.com/RazrFalcon/xmlparser) with a focus on
reducing memory consumption and removing the lifetime parameter from `Token`.

### Differences from upstream

The upstream library represents token fields as `StrSpan<'a>`, a struct that holds a `&'a str`
pointer along with a byte offset into the original document. This ties every `Token<'a>` to the
lifetime of the source string, and each span carries an 8-byte pointer even though the pointer is
always the same (the start of the document).

This fork replaces `StrSpan<'a>` fields inside tokens with two offset-only types:

- **`SmallDetachedStrSpan`** — stores `start` and `end` as `u16` values relative to the token's
  own `start` offset. Used for short in-token strings (tag names, attribute names/values, etc.).
- **`DetachedStrSpan`** — stores `start` and `end` as `u32` absolute offsets. Used where `u16`
  would overflow.

Each token stores one absolute `start: usize` position and a `end: u16` length. All string spans
inside the token are relative to that `start`, so no pointer is stored. To recover a `&str` you
call `span.as_str(full_text, token_start)`.

The result is that `Token` has no lifetime parameter, is `Copy + 'static`, and the token enum
variants are significantly smaller in memory.

---

![Build Status](https://github.com/RazrFalcon/xmlparser/workflows/Rust/badge.svg)
[![Crates.io](https://img.shields.io/crates/v/xmlparser.svg)](https://crates.io/crates/xmlparser)
[![Documentation](https://docs.rs/xmlparser/badge.svg)](https://docs.rs/xmlparser)
[![Rust 1.31+](https://img.shields.io/badge/rust-1.31+-orange.svg)](https://www.rust-lang.org)
![](https://img.shields.io/badge/unsafe-forbidden-brightgreen.svg)

*xmlparser* is a low-level, pull-based, zero-allocation
[XML 1.0](https://www.w3.org/TR/xml/) parser.

### Example

```rust
for token in xmlparser::Tokenizer::from("<tagname name='value'/>") {
    println!("{:?}", token);
}
```

### Why a new library?

This library is basically a low-level XML tokenizer that preserves the positions of the tokens
and is not intended to be used directly.
If you are looking for a higher level solution, check out
[roxmltree](https://github.com/RazrFalcon/roxmltree).

### Benefits

- All tokens contain `StrSpan` structs which represent the position of the substring
  in the original document.
- Good error processing. All error types contain the position (line:column) where it occurred.
- No heap allocations.
- No dependencies.
- Tiny. ~1400 LOC and ~30KiB in the release build according to `cargo-bloat`.
- Supports `no_std` builds. To use without the standard library, disable the default features.

### Limitations

- Currently, only ENTITY objects are parsed from the DOCTYPE. All others are ignored.
- No tree structure validation. So an XML like `<root><child></root></child>`
  or a string without root element
  will be parsed without errors. You should check for this manually.
  On the other hand `<a/><a/>` will lead to an error.
- Duplicated attributes is not an error. So XML like `<item a="v1" a="v2"/>`
  will be parsed without errors. You should check for this manually.
- UTF-8 only.

### Safety

- The library must not panic. Any panic is considered a critical bug
  and should be reported.
- The library forbids unsafe code.

### License

Licensed under either of

- Apache License, Version 2.0
  ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license
  ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
