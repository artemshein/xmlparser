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

### Benchmarks

Measured on Apple Silicon (aarch64) with Rust 1.96. Three XML fixtures:
- **dense** — 44 KB, compact elements, minimal whitespace
- **spaced** — 66 KB, heavy inter-element whitespace
- **dtd** — 12 KB, DOCTYPE with 50 ENTITY declarations, comments, and processing instructions
- **large** — 1 GB, 10.5 million items, same structure as dense

#### Token memory footprint

| | This fork | Upstream |
|---|---|---|
| `size_of::<Token>()` | **24 bytes** | 112 bytes |
| 10.5 M tokens (1 GB file) | **240 MB** | 1 120 MB |
| Savings | **−88 bytes/token (78.6%)** | — |

The fork stores two `u16` offsets per span relative to the token start; upstream stores a full
`&str` (pointer + length = 16 bytes) per span.

#### Streaming throughput (tokens counted, nothing stored)

| File | Fork | Upstream | Δ |
|---|---|---|---|
| dense (44 KB) | 453 MiB/s | 440 MiB/s | **+3%** |
| spaced (66 KB) | 503 MiB/s | 496 MiB/s | **+2%** |
| dtd (12 KB) | 474 MiB/s | 445 MiB/s | **+6%** |
| large (1 GB) | 487 MiB/s | 476 MiB/s | **+2.5%** |

#### Collect throughput (`Vec<Token>` allocation included)

| File | Fork | Upstream | Δ |
|---|---|---|---|
| dense (44 KB) | 424 MiB/s | 342 MiB/s | **+24%** |
| spaced (66 KB) | 471 MiB/s | 400 MiB/s | **+18%** |
| dtd (12 KB) | 433 MiB/s | 358 MiB/s | **+21%** |

The streaming gap (~2–6%) reflects smaller tokens fitting better in registers. The collect gap
(~18–24%) is driven directly by the 4.7× smaller `Vec` — less memory to allocate, write, and
grow during reallocation.

Benchmarks live in `benches/tokenize.rs` and can be reproduced with:

```
cargo bench
```

---

# xmlparser

[<img alt="github" src="https://img.shields.io/badge/github-RazrFalcon/xmlparser-8da0cb?style=for-the-badge&logo=github" height="20">](https://github.com/RazrFalcon/xmlparser)
[<img alt="crates.io" src="https://img.shields.io/crates/v/xmlparser.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/xmlparser)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-xmlparser-66c2a5?style=for-the-badge&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMiA0MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMiAzOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMiA0MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/xmlparser)
[<img alt="build status" src="https://img.shields.io/github/actions/workflow/status/RazrFalcon/xmlparser/ci.yml?branch=master&style=for-the-badge" height="20">](https://github.com/RazrFalcon/xmlparser/actions?query=branch%3Amaster)

*xmlparser* is a low-level, pull-based, zero-allocation
[XML 1.0](https://www.w3.org/TR/xml/) parser.

<br>

## Example

```rust
for token in xmlparser::Tokenizer::from("<tagname name='value'/>") {
    println!("{:?}", token);
}
```

<br>

## Why a new library?

This library is basically a low-level XML tokenizer that preserves the
positions of the tokens and is not intended to be used directly.

If you are looking for a higher level solution, check out
[roxmltree](https://github.com/RazrFalcon/roxmltree).

<br>

## Benefits

- All tokens contain `StrSpan` structs which represent the position of the
  substring in the original document.
- Good error processing. All error types contain the position (line:column)
  where it occurred.
- No heap allocations.
- No dependencies.
- Tiny. ~1400 LOC and ~30KiB in the release build according to
  `cargo-bloat`.
- Supports `no_std` builds. To use without the standard library, disable the
  default features.

<br>

## Limitations

- Currently, only ENTITY objects are parsed from the DOCTYPE. All others are
  ignored.
- No tree structure validation. So an XML like
  `<root><child></root></child>` or a string without root element will be
  parsed without errors. You should check for this manually. On the other
  hand `<a/><a/>` will lead to an error.
- Duplicated attributes is not an error. So XML like `<item a="v1" a="v2"/>`
  will be parsed without errors. You should check for this manually.
- UTF-8 only.

<br>

## Safety

- The library must not panic. Any panic is considered a critical bug and
  should be reported.
- The library forbids unsafe code.

<br>

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE] or
  http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT] or http://opensource.org/licenses/MIT)

at your option.

<br>

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

[LICENSE-APACHE]: https://github.com/RazrFalcon/xmlparser/blob/master/LICENSE-APACHE
[LICENSE-MIT]: https://github.com/RazrFalcon/xmlparser/blob/master/LICENSE-MIT
