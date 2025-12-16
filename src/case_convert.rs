// Vendored from the `const_format` crate at commit `73dbf70`, because that crate
// depends on `syn`, `quote`, etc. but for changing case we don't need those
//
// ORIGINAL LICENSE:
//
// Copyright (c) 2020 Matias Rodriguez.
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

/// This type is passed to `#[zenum(rename_all = zenum::Case::Lower)]` to change the case
/// of all variants that are being serialized.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Case {
    /// lowercase
    Lower,
    /// UPPERCASE
    Upper,
    /// PascalCase
    Pascal,
    /// camelCase
    Camel,
    /// snake_case
    Snake,
    /// SCREAMING_SNAKE_CASE
    ScreamingSnake,
    /// kebab-case
    Kebab,
    /// SCREAMING-KEBAB-CASE
    ScreamingKebab,
}

#[doc(hidden)]
#[macro_export]
macro_rules! __case_from_string_literal {
    ($lit:literal) => {
        const {
            match $lit {
                _ if $crate::private::const_str::equal!("lowercase", $lit) => $crate::Case::Lower,
                _ if $crate::private::const_str::equal!("UPPERCASE", $lit) => $crate::Case::Upper,
                _ if $crate::private::const_str::equal!("camelCase", $lit) => $crate::Case::Camel,
                _ if $crate::private::const_str::equal!("PascalCase", $lit) => $crate::Case::Pascal,
                _ if $crate::private::const_str::equal!("snake_case", $lit) => $crate::Case::Snake,
                _ if $crate::private::const_str::equal!("SCREAMING_SNAKE_CASE", $lit) => $crate::Case::ScreamingSnake,
                _ if $crate::private::const_str::equal!("kebab-case", $lit) => $crate::Case::Kebab,
                _ if $crate::private::const_str::equal!("SCREAMING-KEBAB-CASE", $lit) => $crate::Case::ScreamingKebab,
                _ => panic!(r#"invalid value for `#[zenum(rename_all = "...")]`, expected one of: "lowercase", "UPPERCASE", "camelCase", "PascalCase", "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case" or "SCREAMING-KEBAB-CASE""#),
            }
        }
    };
    // Custom case that the user chose, e.g. `const CASE = Case::Snake`
    ($expr:expr) => {
        $expr
    };
}

#[macro_export]
macro_rules! map_ascii_case {
    ($case:expr, $str:expr) => {
        $crate::private::identity::<&'static str>({
            const L: $crate::private::usize = $case.size_after_conversion($str);

            const BYTES: &[$crate::private::u8; L] =
                &$crate::private::case_convert::convert_str::<L>($case, $str);

            const STR: &$crate::private::str =
                unsafe { $crate::private::str::from_utf8_unchecked(BYTES) };

            STR
        })
    };
}

impl Case {
    /// Returns the size of `s` after we apply the case conversion to it
    pub const fn size_after_conversion(self, s: &str) -> usize {
        struct WordCountAndLength {
            /// The amount of words
            count: usize,
            /// The length of all words added up
            length: usize,
        }

        const fn words_count_and_length(bytes: &[u8]) -> WordCountAndLength {
            let mut count = 0;
            let mut length = 0;
            let mut word_iter = WordIterator::new(bytes);
            while_next_word! {word_iter, word_range => {
                count += 1;
                length += word_range.end - word_range.start;
            }}
            WordCountAndLength { count, length }
        }

        match self {
            Case::Upper | Case::Lower => s.len(),
            Case::Pascal | Case::Camel => {
                let wcl = words_count_and_length(s.as_bytes());
                wcl.length
            }
            Case::Snake | Case::Kebab | Case::ScreamingSnake | Case::ScreamingKebab => {
                let wcl = words_count_and_length(s.as_bytes());
                wcl.length + wcl.count.saturating_sub(1)
            }
        }
    }
}

macro_rules! if_next_word {
    ($word_iterator:ident, $word_range:ident => $then:block $(else $else:block)? ) => {
        #[allow(unused_mut)]
        if let Some((niter, mut $word_range)) = $word_iterator.next() {
            $word_iterator = niter;

            $then
        } $(else $else)?
    };
}

macro_rules! while_next_word {
    ($word_iterator:ident, $word_range:ident => $then:block) => {
        #[allow(unused_mut)]
        while let Some((niter, mut $word_range)) = $word_iterator.next() {
            $word_iterator = niter;

            $then
        }
    };
}
use while_next_word;

use core::fmt::{self, Debug};

macro_rules! for_range_inc {
    ($current:ident in $start:expr, $end:expr => $($code:tt)*) => {
        let mut $current = $start;
        let end = $end;

        while $current <= end {
            $($code)*

            $current+=1;
        }
    };
}

use core::ops::Range;

#[derive(Copy, Clone)]
struct ByteKind(u8);

impl Debug for ByteKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match () {
            _ if self.0 == Self::Other.0 => "Other",
            _ if self.0 == Self::Number.0 => "Number",
            _ if self.0 == Self::LowerCase.0 => "LowerCase",
            _ if self.0 == Self::UpperCase.0 => "UpperCase",
            _ if self.0 == Self::NonAscii.0 => "NonAscii",
            _ => unreachable!(),
        })
    }
}

#[allow(non_upper_case_globals)]
impl ByteKind {
    const Other: Self = Self(0b0001);
    const Number: Self = Self(0b0010);
    const LowerCase: Self = Self(0b0100);
    const UpperCase: Self = Self(0b1000);
    const Alphabetic: Self = Self(Self::LowerCase.0 | Self::UpperCase.0);
    // Assumes that non-ascii chars are mostly alphabetic,
    // this should work out fine most of the time.
    const NonAscii: Self = Self(0b1100);
}

impl ByteKind {
    #[allow(dead_code)]
    #[inline(always)]
    pub const fn eq(self, other: Self) -> bool {
        (self.0 & other.0) != 0
    }

    #[inline(always)]
    pub const fn ne(self, other: Self) -> bool {
        (self.0 & other.0) == 0
    }

    #[inline(always)]
    pub const fn is_alphabetic(self) -> bool {
        self.0 == Self::LowerCase.0 || self.0 == Self::UpperCase.0
    }

    pub const fn is_end_of_word(mut self, prev: Self, other: Self) -> bool {
        if self.0 == Self::NonAscii.0 {
            self = prev;
        }

        if self.0 == Self::UpperCase.0 {
            other.ne(Self::Alphabetic)
        } else {
            self.ne(other)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct WordIterator<'a> {
    bytes: &'a [u8],
    start: usize,
}

const BYTE_KIND: &[ByteKind; 256] = &{
    let mut out = [ByteKind::NonAscii; 256];

    // Make sure that this goes first
    for_range_inc! {i in 0, 127 => out[i as usize] = ByteKind::Other; }
    for_range_inc! {i in b'A', b'Z' => out[i as usize] = ByteKind::UpperCase; }
    for_range_inc! {i in b'a', b'z' => out[i as usize] = ByteKind::LowerCase; }
    for_range_inc! {i in b'0', b'9' => out[i as usize] = ByteKind::Number; }

    out
};

impl<'a> WordIterator<'a> {
    pub(crate) const fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, start: 0 }
    }

    const fn skip_same_kind(mut self, mut kind: ByteKind) -> (Self, ByteKind) {
        let orig_bytes_len = self.bytes.len();

        let mut prev_kind = kind;
        while let [b, rem @ ..] = self.bytes {
            let next_kind = BYTE_KIND[*b as usize];
            let cmp = kind.is_end_of_word(prev_kind, next_kind);
            if kind.is_alphabetic() {
                prev_kind = kind;
            }
            kind = next_kind;
            if cmp {
                break;
            }
            self.bytes = rem;
        }

        // Advance until a char boundary is found
        while let [b, rem @ ..] = self.bytes {
            if (*b as i8) >= -0x40 {
                break;
            }
            self.bytes = rem;
        }

        // Remember not to add return statements to the function
        self.start += orig_bytes_len - self.bytes.len();

        (self, kind)
    }

    pub(crate) const fn next(self) -> Option<(Self, Range<usize>)> {
        let (this, fkind) = self.skip_same_kind(ByteKind::Other);
        if this.bytes.is_empty() {
            None
        } else {
            let (next, _) = this.skip_same_kind(fkind);
            let range = this.start..next.start;
            Some((next, range))
        }
    }
}

pub const fn convert_str<const N: usize>(case: Case, s: &str) -> [u8; N] {
    let mut arr = [0; N];
    let mut inp = s.as_bytes();
    let mut o = 0;

    macro_rules! map_bytes {
        ($byte:ident => $e:expr) => {
            while let [$byte, rem @ ..] = inp {
                let $byte = *$byte;
                inp = rem;
                arr[o] = $e;
                o += 1;
            }
        };
    }

    macro_rules! write_byte {
        ($byte:expr) => {
            arr[o] = $byte;
            o += 1;
        };
    }

    macro_rules! write_range_from {
        ($range:expr, $from:expr, $byte:ident => $mapper:expr) => {{
            let mut range = $range;
            while range.start < range.end {
                let $byte = $from[range.start];
                arr[o] = $mapper;

                range.start += 1;
                o += 1;
            }
        }};
    }

    macro_rules! write_snake_kebab_case {
        ($separator:expr, $byte_conversion:expr) => {{
            let mut word_iter = WordIterator::new(inp);

            if_next_word! {word_iter, word_range => {
                write_range_from!(word_range, inp, byte => $byte_conversion(byte));

                while_next_word!{word_iter, word_range => {
                    write_byte!($separator);
                    write_range_from!(word_range, inp, byte => $byte_conversion(byte));
                }}
            }}
        }};
    }

    macro_rules! write_pascal_camel_case {
        ($first_word_conv:expr) => {{
            let mut word_iter = WordIterator::new(inp);

            if_next_word! {word_iter, word_range => {
                write_byte!($first_word_conv(inp[word_range.start]));
                word_range.start += 1;
                write_range_from!(word_range, inp, byte => lowercase_u8(byte));

                while_next_word!{word_iter, word_range => {
                    write_byte!(uppercase_u8(inp[word_range.start]));
                    word_range.start += 1;
                    write_range_from!(word_range, inp, byte => lowercase_u8(byte));
                }}
            }}
        }};
    }

    match case {
        Case::Upper => map_bytes!(b => uppercase_u8(b)),
        Case::Lower => map_bytes!(b => lowercase_u8(b)),
        Case::Snake => write_snake_kebab_case!(b'_', lowercase_u8),
        Case::ScreamingSnake => write_snake_kebab_case!(b'_', uppercase_u8),
        Case::Kebab => write_snake_kebab_case!(b'-', lowercase_u8),
        Case::ScreamingKebab => write_snake_kebab_case!(b'-', uppercase_u8),
        Case::Pascal => write_pascal_camel_case!(uppercase_u8),
        Case::Camel => write_pascal_camel_case!(lowercase_u8),
    }

    arr
}

const CASE_DIFF: u8 = b'a' - b'A';

const fn uppercase_u8(b: u8) -> u8 {
    if let b'a'..=b'z' = b {
        b - CASE_DIFF
    } else {
        b
    }
}

const fn lowercase_u8(b: u8) -> u8 {
    if let b'A'..=b'Z' = b {
        b + CASE_DIFF
    } else {
        b
    }
}
