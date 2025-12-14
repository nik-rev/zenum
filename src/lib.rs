// This is needed only for optimizations.
// We could run this at runtime instead and the compiler would probably optimize it. but
// also probably not. So since we're using nightly anyway, let's benefit from it
#![feature(const_option_ops)]
#![feature(const_trait_impl)]
#![feature(const_cmp)]
#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]
#![allow(clippy::crate_in_macro_def, reason = "clippy bug")]
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
    pub use None;
    pub use Some;
    pub use const_format;
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
            // Entry point. \$_ _ field is the field we're looking for in the list of #[$_ _ attr]ibutes
            ($_ field:ident: $_ (#[$_ ($_ attr:tt)*])*) => {
                $_ (
                    // Try to extract from each individual `#[attr]`
                    if let $_ crate::private::Some(val) = $_ crate::$macro_name!(@ $_ field: $_ ($_ attr)*) {
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
            (@ $_ field:ident: $helper_attr_name($_ ($_ attr:tt)*)) => { $_ crate::$macro_name!(! $_ field: $_ ($_ attr)*) };
            // Any other field is totally ignored, e.g. `#[serde(rename_all = "kebab-case")]`
            (@ $_ field:ident: $_ ($_ ignore:tt)*) => { $_ crate::private::None };
            //
            // SUPPORTED FIELDS, AND what we are looking for
            //
            $(
                // this value is at the start or in the middle of the input
                (! $value_ident: $value_ident $($value)*, $_ ($_ attr:tt)+) => { $_ crate::private::Some($($captured)*) };
                // this value is at the end of the input
                (! $value_ident: $value_ident $($value)* $_ (,)?) => { $_ crate::private::Some($($captured)*) };
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

define_extract_macro! {$
    macro_rules! extract_field;

    match ? in #[zenum(?)] {
        { rename_all = $value:expr } => { $value }
        { disabled } => { () }
        { rename = $value:expr } => { $value }
        { aliases = $value:expr } => { $value }
    }
}

#[cfg(false)]
/// Takes a bunch of attributes, and finds the first field in our helper macro
///
/// For example, a valid input to this macro is this:
///
/// ```ignore
/// #[foo]
/// #[bar]
/// #[zenum(rename_all = "kebab-case")]
/// #[baz]
/// ```
///
/// It will then find the first `rename_all` attribute that it finds, and
/// evaluate to its value, in this case `"kebab-case"`
//
// NOTE: We could generate this macro with a macro, to reduce boilerplate when adding new fields
// but I don't think it would be worth it in terms of readability
#[doc(hidden)]
#[macro_export]
macro_rules! extract_field {
    // Entry point. $field is the field we're looking for in the list of #[$attr]ibutes
    ($field:ident: $(#[$($attr:tt)*])*) => {
        $(
            // Try to extract from each individual `#[attr]`
            if let Some(val) = $crate::extract_field!(@ $field: $($attr)*) {
                Some(val)
            } else
        )* {
            None
        }
    };
    // We only want to parse this part
    //
    // #[zenum(rename_all = "kebab-case")]
    //         ^^^^^^^^^^^^^^^^^^^^^^^^^
    (@ $field:ident: zenum($($attr:tt)*)) => { $crate::extract_field!(! $field: $($attr)*) };
    // Any other field is totally ignored, e.g. `#[serde(rename_all = "kebab-case")]`
    (@ $field:ident: $($ignore:tt)*) => { None };
    //
    // SUPPORTED FIELDS, AND what we are looking for
    //
    // odd = this value is at the start or in the middle of the input
    // even = this value is at the end of the input
    //
    (! rename_all: rename_all = $value:expr, $($attr:tt)+) => { Some($value) };
    (! rename_all: rename_all = $value:expr $(,)?) => { Some($value) };
    (! disabled: disabled, $($attr:tt)+) => { Some(()) };
    (! disabled: disabled $(,)?) => { Some(()) };
    (! rename: rename = $value:expr, $($attr:tt)+) => { Some($value) };
    (! rename: rename = $value:expr $(,)?) => { Some($value) };
    (! aliases: aliases = $value:expr, $($attr:tt)+) => { Some($value) };
    (! aliases: aliases = $value:expr $(,)?) => { Some($value) };
    //
    // SUPPORTED FIELDS, not what we are looking for
    //
    // odd = this value is at the start or in the middle of the input
    // even = this value is at the end of the input
    //
    (! $field:ident: rename_all = $value:expr, $($attr:tt)+) => { $crate::extract_field!(! $field: $($attr)+) };
    (! $field:ident: rename_all = $value:expr $(,)?) => { None };
    (! $field:ident: disabled, $($attr:tt)+) => { $crate::extract_field!(! $field: $($attr)*) };
    (! $field:ident: disabled $(,)?) => { None };
    (! $field:ident: rename = $value:expr, $($attr:tt)+) => { $crate::extract_field!(! $field: $($attr)+) };
    (! $field:ident: rename = $value:expr $(,)?) => { None };
    (! $field:ident: aliases = $value:expr, $($attr:tt)+) => { $crate::extract_field!(! $field: $($attr)+) };
    (! $field:ident: aliases = $value:expr $(,)?) => { None };
    //
    // CATCHALL: if we go here, it's an error. Unrecognized token
    //
    (! $field:ident: $ignore:ident $($attr:tt)*) => {
        compile_error!(concat!(
            "unexpected token: `",
            stringify!($ignore),
            "`, allowed `#[zenum(/* ... */)]` arguments are:\n",
            "• `rename_all = $_:expr`\n",
            "• `disabled`\n",
            "• `rename = $_:expr`\n"
            "• `aliases = $_:expr`\n"
        ))
    };
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
    // #[stren(serialize = "blue", serialize = "b")]
    Blue(usize),

    // Notice that we can disable certain variants from being found
    #[zenum(disabled)]
    Yellow,

    // We can make the comparison case insensitive (however Unicode is not supported at the moment)
    // #[stren(ascii_case_insensitive)]
    Black,
}

fn main() {
    println!("Hello, world!");
}
