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

pub mod private {
    pub use const_format;
}

fn lol(s: &str) {
    const X: &str = {
        match "x" {
            "a" => "z",
            "b" => "y",
            _ => "m",
        }
    };
    match s {
        X => (),
        _ => todo!(),
    }
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

macro_rules! EnumString {
    derive() (
        $(#[$($enum_attr:tt)*])*
        $enum_vis:vis enum $enum_ident:ident {
            $(
                $(#[$($enum_variant_attr:tt)*])*
                $enum_variant:ident
                    // enum with named fields
                    $({
                        $(
                            $(#[$($enum_variant_named_field_attr:tt)*])*
                            $enum_variant_named_field_ident:ident: $enum_variant_named_field_ty:ty
                        ),* $(,)?
                    })?
                    // enum with unnamed fields
                    $((
                        $(
                            $(#[$($enum_variant_unnamed_field_attr:tt)*])*
                            $enum_variant_unnamed_field_ty:ty
                        ),* $(,)?
                    ))?
                    // discriminant
                    $(= $enum_variant_discriminant:expr)?
            ),* $(,)?
        }
    ) => {
        impl ::core::str::FromStr for $enum_ident {
            type Err = ();

            fn from_str(s: &::core::primitive::str) -> ::core::result::Result<Self, Self::Err> {
                const STREN_RENAME_ALL: Option<$crate::private::const_format::Case> = {
                    match $crate::extract_field!(rename_all, $(#[$($enum_attr)*])*) {
                        Some("lowercase") => Some($crate::private::const_format::Case::Lower),
                        Some("UPPERCASE") => Some($crate::private::const_format::Case::Upper),
                        Some("PascalCase") => Some($crate::private::const_format::Case::Pascal),
                        Some("camelCase") => Some($crate::private::const_format::Case::Camel),
                        Some("snake_case") => Some($crate::private::const_format::Case::Snake),
                        Some("SCREAMING_SNAKE_CASE") => Some($crate::private::const_format::Case::UpperSnake),
                        Some("kebab-case") => Some($crate::private::const_format::Case::Kebab),
                        Some("SCREAMING-KEBAB-CASE") => Some($crate::private::const_format::Case::UpperKebab),
                        Some(unknown) => panic!("invalid value for `#[stren(rename_all_fields)]`"),
                        None => None
                    }
                };
                $(
                    #[allow(non_upper_case_globals)]
                    const $enum_variant: (&str, bool) = {
                        const FIELD_STR: &str = stringify!($enum_ident);

                        let rename: Option<&str> = $crate::extract_field!(rename, $(#[$($enum_variant_attr)*])*);

                        // actual string representation of the enum variant
                        let name = if let Some(rename) = rename {
                            // supplied #[stren(rename = "blabla")]
                            rename
                        } else if STREN_RENAME_ALL.is_some() {
                            // Apply "rename-all" rule to this string

                            // this is very bad, we have to do it like this because `if let Some(s) = Y` with `s`
                            // cannot be used in a `const` context
                            //
                            // Apparently every single branch gets evaluated in `const`, so we must make it always compile.
                            // The `or` path will never actually be taken
                            $crate::private::const_format::map_ascii_case!(STREN_RENAME_ALL.unwrap_or($crate::private::const_format::Case::Lower), FIELD_STR)
                        } else {
                            // default to stringified name of the enum variant
                            FIELD_STR
                        };
                        let is_disabled: Option<()> = $crate::extract_field!(disabled, $(#[$($enum_variant_attr)*])*);

                        (name, is_disabled.is_some())
                    };
                )*
                match s {
                    $(
                        v if v == $enum_variant.0
                        // not #[stren(disabled)]
                        && !$enum_variant.1 => Ok(Self::$enum_variant
                            // enum with named fields
                            $({
                                $(
                                    $enum_variant_named_field_ident:
                                    <$enum_variant_named_field_ty as ::core::default::Default>::default(),
                                )*
                            })?
                            // enum with unnamed fields
                            $((
                                $(
                                    <$enum_variant_unnamed_field_ty as ::core::default::Default>::default(),
                                )*
                            ))?
                        ),
                    )*
                    _ => Err(())
                }
            }
        }
    };
}

use struz_helpers::dummy;

#[derive(Debug, PartialEq, EnumString, dummy)]
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
