/// This macro will try to automatic add the trait [`Desaturated`] to a function signature.
#[cfg_attr(
    all(
        feature = "macros",
        feature = "async",
        feature = "blocking",
        feature = "std"
    ),
    doc = r#"
```
use desaturate::{Blocking, desaturate, IntoDesaturated, IntoDesaturatedWith, Desaturated, boxed::BoxedDesaturated};
#[desaturate]
async fn do_something(arg: i32) -> i32 {
    other_function(arg).await
}

#[desaturate]
async fn other_function(arg: i32) -> i32 {
    arg * 2
}

# #[tokio::main]
async fn main() {
    assert_eq!(do_something(5).await, do_something(5).call())
}
```"#
)]
///
/// ## Available attributes:
///
/// ```
/// # use desaturate::{desaturate, Blocking};
/// // Dump the result generated code into stderr:
/// #[desaturate(debug_dump)]
/// # async fn do_nothing() {}
/// # struct Test<'a>(&'a i32);
/// # impl<'a> Test<'a> {
///
/// // Use the lifetime 'a instead of a generated one for the lifetime of impl Desaturated<_>:
/// #[desaturate(lifetime = "'a")]
/// # async fn inner(&self) -> &i32 { self.0 }
/// # }
///
/// // Have conditional compilation based on current function color:
/// #[desaturate(only_blocking_attr = "for_blocking", only_async_attr = "for_async")]
/// async fn target_specific(arg: i32) -> i32 {
///     let something;
///     #[for_blocking] { something = arg + 1 }
///     #[for_async] { something = arg - 1 }
///     something
/// }
/// #
/// # #[tokio::main]
/// # async fn main() {
/// # assert_eq!(6, target_specific(5).call());
/// # assert_eq!(4, target_specific(5).await);
/// # }
/// ```
/// Multiple attributes can be added with comma separation.
pub use desaturate_macros::*;
