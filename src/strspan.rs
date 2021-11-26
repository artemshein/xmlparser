use core::fmt;
use core::ops::{Deref, Range};

/// A string slice, holding offsets only.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct DetachedStrSpan {
    start: u32,
    end: u32,
}

impl DetachedStrSpan {
    /// Slice the string using this span
    pub fn as_str<'a>(&self, s: &'a str, offset: usize) -> &'a str {
        &s[offset + self.start as usize..offset + self.end as usize]
    }

    /// Start offset
    pub fn start(&self) -> u32 {
        self.start
    }

    /// End offset
    pub fn end(&self) -> u32 {
        self.end
    }
}

/// A string slice, holding offsets only.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SmallDetachedStrSpan {
    start: u16,
    end: u16,
}

impl SmallDetachedStrSpan {
    /// Slice the string using this span
    pub fn as_str<'a>(&self, s: &'a str, offset: usize) -> &'a str {
        &s[offset + self.start as usize..offset + self.end as usize]
    }

    /// Start offset
    pub fn start(&self) -> u16 {
        self.start
    }

    /// End offset
    pub fn end(&self) -> u16 {
        self.end
    }
}

/// A string slice.
///
/// Like `&str`, but also contains the position in the input XML
/// from which it was parsed.
#[must_use]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrSpan<'a> {
    text: &'a str,
    start: usize,
}

impl<'a> From<&'a str> for StrSpan<'a> {
    #[inline]
    fn from(text: &'a str) -> Self {
        StrSpan { text, start: 0 }
    }
}

impl<'a> StrSpan<'a> {
    /// Constructs a new `StrSpan` from substring.
    #[inline]
    pub(crate) fn from_substr(text: &str, start: usize, end: usize) -> StrSpan {
        debug_assert!(start <= end);
        StrSpan {
            text: &text[start..end],
            start,
        }
    }

    /// Make a detached span for span
    pub fn detach(&self, offset: usize) -> DetachedStrSpan {
        if self.start == self.end() {
            return DetachedStrSpan { start: 0, end: 0 };
        }
        debug_assert!(offset <= self.start);
        debug_assert!(self.end() >= self.start);
        DetachedStrSpan {
            start: (self.start - offset) as u32,
            end: (self.end() - offset) as u32,
        }
    }

    /// Make a detached span for span
    pub fn detach_small(&self, offset: usize) -> SmallDetachedStrSpan {
        if self.start == self.end() {
            return SmallDetachedStrSpan { start: 0, end: 0 };
        }
        debug_assert!(offset <= self.start);
        debug_assert!(self.end() >= self.start);
        SmallDetachedStrSpan {
            start: (self.start - offset) as u16,
            end: (self.end() - offset) as u16,
        }
    }

    /// Returns the start position of the span.
    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    /// Returns the end position of the span.
    #[inline]
    pub fn end(&self) -> usize {
        self.start + self.text.len()
    }

    /// Returns the range of the span.
    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start..self.end()
    }

    /// Returns the span as a string slice
    #[inline]
    pub fn as_str(&self) -> &'a str {
        &self.text
    }

    /// Returns an underling string region as `StrSpan`.
    #[inline]
    pub(crate) fn slice_region(&self, start: usize, end: usize) -> StrSpan<'a> {
        StrSpan::from_substr(self.text, start, end)
    }
}

impl<'a> fmt::Debug for StrSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "StrSpan({:?} {}..{})",
            self.as_str(),
            self.start(),
            self.end()
        )
    }
}

impl<'a> fmt::Display for StrSpan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl<'a> Deref for StrSpan<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.text
    }
}
