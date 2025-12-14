// This is needed only for optimizations.
// We could run this at runtime instead and the compiler would probably optimize it. but
// also probably not. So since we're using nightly anyway, let's benefit from it
#![feature(const_option_ops)]
#![feature(const_trait_impl)]
#![feature(const_cmp)]
//
#![feature(macro_derive)]
#![feature(macro_attr)]
//! `enumwow`
//!
//! # Differences from `strum`
//!
//! Creating a second crate allows us to re-think the entire API, and we did:
//!
//! - `FromStr`
//! - `Display`
//! - `VARIANT_COUNT` adds associated constant `VARIANT_COUNT`
//! - `VARIANT_NAMES` adds associated constant `VARIANT_NAMES`
//! - `EnumKind`, creates a same enum without any fields
//! - `EnumIter`, creates an iterator over the enum's variants (fields are `Default`ed)
//! - `from_discriminant`
//! - `discriminant`
//! - `variant_name`, a function of signature `fn(&self) -> &'static str`
//! - `is` for each variant, returns `bool`
//! - `as`, get the exact variant as an `Option`
//! - `map`, get the exact variant in a closure
//! - `unwrap`
//! - `expect`
//!
//! wowenum
//!
//! - `EnumString` renamed to `FromStr`
//! - Generics are not supported, due of fundamental language limitations
//!   in Rust's `macro_rules!` macros
//! - No traits. The fact that `strum` requires importing its special traits
//!   makes the APIs harder to use, and I've yet to see a single library that actually
//!   was generic over any of `strum`'s traits, so instead we add the respective
//!   items as associated constants/functions

mod as_variant;
mod display;
mod enum_iter;
mod enum_kind;
mod expect_variant;
mod from_str;
mod is_variant;
mod map_variant;
mod unwrap_variant;
mod variant_count;
mod variant_names;

// Not public API.
#[doc(hidden)]
pub mod private {
    pub use const_format;
}

/// Takes a bunch of attributes, and finds the given field
///
/// For example, a valid input to this macro is this:
///
/// ```ignore
/// #[foo]
/// #[bar]
/// #[stren(rename_all = "kebab-case")]
/// #[baz]
/// ```
///
/// It will then find the first `rename_all` attribute that it finds, and
/// evaluate to its value, in this case `"kebab-case"`
#[doc(hidden)]
#[macro_export]
macro_rules! extract_field {
    ($field:ident, $(#[$($attr:tt)*])*) => {{
        $(
            if let Some(val) = $crate::extract_field!(@ $field, $($attr)*) {
                Some(val)
            } else
        )* {
            None
        }
    }};
    (@ $field:ident, stren($($attr:tt)*)) => {
        $crate::extract_field!(! $field, $($attr)*)
    };
    (@ $field:ident, $($ignore:tt)*) => {};
    //
    // SUPPORTED FIELDS
    //
    (! rename_all, rename_all = $value:literal $($attr:tt)*) => { Some($value) };
    (! disabled, disabled $($attr:tt)*) => { Some(()) };
    (! rename, rename = $value:literal $($attr:tt)*) => { Some($value) };
    //
    (! $field:ident, $ignore:tt $($attr:tt)*) => {
        $crate::extract_field!(! $field, $($attr)*)
    };
    (! $field:ident,) => { None };
}

pub use enumwow_helpers::dummy;

#[derive(Debug, PartialEq, FromStr, dummy)]
enum Color {
    Red,
    // The Default value will be inserted into range if we match "Green".
    Green {
        range: usize,
    },

    // We can match on multiple different patterns.
    #[stren(serialize = "blue", serialize = "b")]
    Blue(usize),

    // Notice that we can disable certain variants from being found
    #[stren(disabled)]
    Yellow,

    // We can make the comparison case insensitive (however Unicode is not supported at the moment)
    #[stren(ascii_case_insensitive)]
    Black,
}

fn main() {
    println!("Hello, world!");
}
