// This is needed only for optimizations.
// We could run this at runtime instead and the compiler would probably optimize it. but
// also probably not. So since we're using nightly anyway, let's benefit from it
#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]
#![allow(clippy::crate_in_macro_def, reason = "clippy bug")]
//
#![feature(macro_metavar_expr)]
#![feature(macro_metavar_expr_concat)]
#![feature(macro_derive)]
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

pub use case_convert::Case;

#[doc(hidden)]
pub mod case_convert;

// Not public API.
#[doc(hidden)]
pub mod private {
    pub use super::case_convert;
    pub use super::from_str::EnumVariantData;
    pub use None;
    pub use Some;
    pub use const_str;
    pub use std::convert::identity;
    pub use str;
    pub use u8;
    pub use usize;
}

/// A meta-macro that generates an attribute parsing macro (like `extract_field`).
///
/// It takes a name for the generated macro, a helper attribute name (like `zenum`),
/// and a list of allowed attributes to parse.
///
/// Usage:
/// ```ignore
/// $crate::define_extract_macro! {
///     extract_field,
///     zenum,
///     [
///         // Value attributes (name = $value:expr)
///         rename_all, rename, aliases,
///
///         // Flag attributes (name)
///         disabled
///     ]
/// }
/// ```
#[macro_export]
macro_rules! define_extract_macro {
    (
        // Escaped `$`, same as `$$` which is currently nightly
        $_:tt
        // the `macro_rules!` to generate
        macro_rules! $macro_name:ident;

        match ? in #[$helper_attr_name:ident(?)] {
            $(
                { $value_ident:ident $($value:tt)* } => { $($captured:tt)* }
            )*
        }
    ) => {
        #[doc(hidden)]
        #[macro_export]
        macro_rules! $macro_name {
            // Entry point. $_ field is the field we're looking for in the list of #[$_ attr]ibutes
            ($_($_ Derive:ident)?, $_ field:ident: $_ (#[$_ ($_ attr:tt)*])*) => {
                $_ (
                    // Try to extract from each individual `#[attr]`
                    if let $_ crate::private::Some(val) = $_ crate::$macro_name!(@ $_($_ Derive:ident)?, $_ field: $_ ($_ attr)*) {
                        $_ crate::private::Some(val)
                    } else
                )* {
                    $_ crate::private::None
                }
            };
            // We only want to parse this part
            //
            // #[zenum(rename_all = "kebab-case")]
            //         ^^^^^^^^^^^^^^^^^^^^^^^^^
            (@ $_($_ Derive:ident)?, $_ field:ident: $helper_attr_name($_ ($_ attr:tt)*)) => { $_ crate::$macro_name!(! $_ field: $_ ($_ attr)*) };
            // Any other field is totally ignored, e.g. `#[serde(rename_all = "kebab-case")]`
            (@ $_($_ Derive:ident)?, $_ field:ident: $_ ($_ ignore:tt)*) => { $_ crate::private::None };
            //
            // SUPPORTED FIELDS, AND what we are looking for
            //
            $(
                // this value is at the start or in the middle of the input
                (! $_($_ Derive:ident)?, $value_ident: $value_ident $($value)*, $_ ($_ attr:tt)+) => { $_ crate::private::Some($($captured)*) };
                // this value is at the end of the input
                (! $_($_ Derive:ident)?, $value_ident: $value_ident $($value)* $_ (,)?) => { $_ crate::private::Some($($captured)*) };
            )*
            //
            // SUPPORTED FIELDS, not what we are looking for
            //
            $(
                // this value is at the start or in the middle of the input
                (! $_ field:ident: $value_ident $($value)*, $_ ($_ attr:tt)+) => { $_ crate::$macro_name!(! $_ field: $_ ($_ attr)+) };
                // this value is at the end of the input
                (! $_ field:ident: $value_ident $($value)* $_ (,)?) => { $_ crate::private::None };
            )*
            //
            // CATCHALL: if we go here, it's an error. Unrecognized token
            //
            (! $_ field:ident: $_ ignore:ident $_ ($_ attr:tt)*) => {
                compile_error!(concat!(
                    "unexpected token: `",
                    stringify!($_ ignore),
                    "`, allowed `#[", stringify!($macro_name), "(/* ... */)]` arguments are:\n",
                    $(
                        "• ", "`",  stringify!($value_ident $($value)*), "`\n",
                    )*
                ))
            };
        }
    };
}

#[macro_export]
macro_rules! unwrap_or {
    ($unwrap:expr, $or:expr) => {
        $unwrap
    };
    (, $or:expr) => {
        $or
    };
}

// #[derive(zenum::FromStr)]
// #[derive(zenum::Display)]
//
// #[derive(zenum::VARIANT_COUNT)]
// #[derive(zenum::VARIANT_NAMES)]
// #[derive(zenum::VARIANTS)]
//
// #[derive(zenum::unwrap)]: generates `unwrap_*` for each variant
// #[derive(zenum::expect)]: generates `expect_*` for each variant
// #[derive(zenum::is)]: generates `is_*` for each variant
// #[derive(zenum::as_)]: generates `as_*` for each variant
// #[derive(zenum::map)]: generates `map_*` for each variant

define_extract_macro! {$
    macro_rules! get_macro_helper_value_for_enum;

    match ? in #[zenum(?)] {
        // Rename all to use a specific case convention
        //
        // Applies to: `FromStr` and `Display`
        { rename_all = $case_convention:expr } => { $crate::__case_from_string_literal!($case_convention) }
        // Append a suffix to all variants
        //
        // Applies to: `FromStr` and `Display`
        { suffix = $suffix:expr } => { $suffix }
        // Prepend a prefix to all variants
        //
        // Applies to: `FromStr` and `Display`
        { prefix = $prefix:expr } => { $prefix }
        // Whether the comparisons for this enum's variants should be case-insensitive
        { case_insensitive } => { () }
    }
}

define_extract_macro! {$
    macro_rules! get_macro_helper_value_for_variant;

    match ? in #[zenum(?)] {
        // if any other branches fail then this variant will be filled
        //
        // Applies to: `FromStr`
        { default } => { () }
        // Inner field's implementation should be used
        //
        // Works only on variants with a single field
        //
        // Applies to: `FromStr` and `Display`
        { transparent } => { () }
        // Removes variant from the generated code.
        //
        // TODO: allow passing identifier of the current macro
        //       to specify list to skip
        //
        // Applies to: Everything, but individual can be skipped e.g. `skip(FromStr, Display)`
        { skip } => { () }
        // Use this name when serializing/deserializing this variant
        //
        // Applies to: `FromStr` and `Display`
        { rename = $value:expr } => { $value }
        // This also allows these values to map to the variant.
        //
        // Must be a list of strings
        //
        // Applies to: `FromStr`
        { aliases = $value:expr } => { $value }
        // Whether the comparisons for this variant should be case-insensitive
        //
        // Applies to: `FromStr`
        { case_insensitive } => { () }
    }
}

define_extract_macro! {$
    macro_rules! helper_value_field;

    match ? in #[zenum(?)] {
    }
}

pub use enumwow_helpers::dummy;

/// Rust compiler will complain about `if FOO.is_some() { const { FOO.unwrap() } }` so
/// we have to do it with a kind of "default" value, even though that value must never actually be reached
///
/// In const-eval, all branches are evaluated: even stuff like `if const { false } { panic!() }` will panic
#[macro_export]
macro_rules! __hacky_unwrap {
    ($expr:expr, HACK = $hack:expr) => {{
        match $expr {
            Some(expr) => expr,
            None => $hack,
        }
    }};
}

mod foo {
    use super::FromStr;
    use super::dummy;

    #[derive(Debug, PartialEq, FromStr, dummy)]
    // #[zenum(rename_all = "lowercase")]
    enum Color {
        Red,
        // The Default value will be inserted into range if we match "Green".
        Green {
            range: usize,
        },

        // We can match on multiple different patterns.
        // #[stren(serialize = "blue", serialize = "b")]
        Blue(usize),

        // Notice that we can disable certain variants from being found
        #[zenum(skip)]
        Yellow,

        // We can make the comparison case insensitive (however Unicode is not supported at the moment)
        // #[stren(ascii_case_insensitive)]
        Black,
    }
}

fn main() {
    println!("Hello, world!");
}
