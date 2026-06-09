extern crate afl;
extern crate xmltok;

use std::str;

use afl::fuzz;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(text) = str::from_utf8(data) {
            for _ in xmltok::Tokenizer::from(text) {}
        }
    });
}
