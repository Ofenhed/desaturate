//! ### Recursion
#![cfg_attr(
    all(feature = "async", feature = "blocking", feature = "macros"),
    doc = r#"
This code may look innocent, but it will fail to compile with error [`E0720`]:
```compile_fail
# use desaturate::desaturate;
#[desaturate]
async fn recursive(arg: i32) -> i32 {
    if arg <= 1 {
        1
    } else {
        arg * recursive(arg - 1).await
    }
}
```
The [`impl Desaturated<_>`] patterns removes the ability to perform recursive calls. To perform
recursion, we must use `Box<dyn _>`. Unfortunately, as this uses dynamic dispatch, this comes with
a small runtime performance cost.
```
# use desaturate::{Blocking, desaturate, IntoDesaturatedWith, Desaturated, boxed::{BoxedDesaturated, BoxedDesaturatedWith}};
# use core::{pin::Pin, future::Future};

#[desaturate]
async fn recursive(arg: i32) -> i32 {
    if arg <= 1 {
        1
    } else {
        let dyn_recursive =
            recursive
                .boxed_future_with()
                .desaturate_with(arg - 1,
                                 recursive.boxed_blocking_with());
        arg * dyn_recursive.await
    }
}

# #[tokio::main]
async fn main() {
    assert_eq!(recursive(5).await, recursive(5).call())
}
```
[`impl Desaturated<_>`]: Desaturated
[`E0720`]: https://doc.rust-lang.org/error_codes/E0720.html
"#
)]
use crate::{macros::features, private::Sealed, Desaturated};
use core::{future::Future, pin::Pin};

impl<O, T: Sealed<O>> Sealed<O> for Box<T> {}

/// A trait to enable dynamic dispatching for [`desaturate()`].
///
/// [`desaturate()`]: crate::IntoDesaturated::desaturate
pub trait BoxedDesaturated<'a, Out> {
    type BoxedFuture: Future<Output = Out>;
    type BoxedBlocking: FnOnce() -> Out;
    fn boxed_future(self) -> Self::BoxedFuture;
    fn boxed_blocking(self) -> Self::BoxedBlocking;
}
impl<'a, Out, D: Desaturated<Out> + 'a> BoxedDesaturated<'a, Out> for D {
    type BoxedFuture = Pin<Box<dyn Future<Output = Out> + 'a>>;
    #[inline(always)]
    fn boxed_future(self) -> Self::BoxedFuture {
        features! {async:
            Box::pin(self.into_future())
        }
        features! {!async:
            use core::future::IntoFuture;
            Box::pin(async {panic!(r#"Called a the async function of a Desaturated without the "async" feature flag"#) }.into_future())
        }
    }
    type BoxedBlocking = Box<dyn FnOnce() -> Out + 'a>;
    #[inline(always)]
    fn boxed_blocking(self) -> Self::BoxedBlocking {
        features! {fn:
            Box::new(move || {
                self.call()
            })
        }
        features! {!fn:
            Box::new(|| panic!(r#"Called a the blocking function of a Desaturated without the "blocking" feature flag"#))
        }
    }
}
/// A trait to enable dynamic dispatching for [`desaturate_with()`].
///
/// [`desaturate_with()`]: crate::IntoDesaturated::desaturate
pub trait BoxedDesaturatedWith<'a, Args, Out> {
    type BoxedFuture: FnOnce(Args) -> Pin<Box<dyn Future<Output = Out> + 'a>>;
    features! {fn:
        type BoxedBlocking: FnOnce(Args) -> Out;
    }
    features! {!fn:
        type BoxedBlocking;
    }
    fn boxed_future_with(self) -> Self::BoxedFuture;
    fn boxed_blocking_with(self) -> Self::BoxedBlocking;
}
impl<'a, Out, Args: 'a, D: Desaturated<Out> + 'a, F: FnOnce(Args) -> D + 'a>
    BoxedDesaturatedWith<'a, Args, Out> for F
{
    type BoxedFuture = Box<dyn FnOnce(Args) -> Pin<Box<dyn Future<Output = Out> + 'a>> + 'a>;
    type BoxedBlocking = Box<dyn FnOnce(Args) -> Out + 'a>;
    #[inline(always)]
    fn boxed_future_with(self) -> Self::BoxedFuture {
        features! {async:
            Box::new(move |args| Box::pin(self(args).into_future()))
        }
        features! {!async:
            use core::future::IntoFuture;
            Box::new(|_| Box::pin(async { panic!(r#"Called a the async function of a DesaturatedWith without the "async" feature flag"#) }.into_future()))
        }
    }
    #[inline(always)]
    fn boxed_blocking_with(self) -> Self::BoxedBlocking {
        features! {fn:
            Box::new(move |args| self(args).call())
        }
        features! {!fn:
            Box::new(|_| panic!(r#"Called a the blocking function of a BoxedDesaturatedWith without the "blocking" feature flag"#))
        }
    }
}
