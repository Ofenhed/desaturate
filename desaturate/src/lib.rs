#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]
#![cfg_attr(
    not(all(
        feature = "std",
        feature = "macros",
        feature = "async",
        feature = "blocking"
    )),
    doc = r#"<div class="warning">This documentation will be incomplete, because of missing feature flags!</div>"#
)]
#![cfg_attr(
    all(feature = "macros", feature = "async", feature = "blocking"),
    doc = r#"
This crate aims at reducing the coloring of rust functions, by simplifying the process of
creating functions which functions both as async and as blocking functions. This is performed
with the [`Desaturated`] trait, which implements [`IntoFuture`] and [`Blocking`], depending on
which feature flags are set.

The idea is that a desaturated function can be created by combining the `async` color with the
blocking color, as such:
```
use desaturate::{Desaturated, IntoDesaturated};
fn do_something() -> impl Desaturated<()> {
    async {
        // Do something here
    }.desaturate(|| {
        // Do the same thing here
    })
}
```

Now, this doesn't reduce code duplication, per se, but it can enable it, especially when used
with the [`desaturate`] macro. This allows us to create functions like this:
```
use desaturate::{desaturate, Blocking};

#[desaturate]
async fn do_the_thing(arg: i32) -> i32 {
    arg * 2
}

#[desaturate]
async fn do_something(arg1: i32, arg2: i32) -> i32 {
    do_the_thing(arg1).await + do_the_thing(arg2).await
}

fn main() {
    let result = do_something(5, 10).call();
    println!("I got to play with the async functions, and got {result}");
}
```

This also takes care of lifetimes, so you can make functions which track (or ignore) lifetimes.
```
use desaturate::{desaturate, Blocking};

#[desaturate]
async fn add_1(i: &mut i32) -> i32 {
    let old = *i;
    *i += 1;
    old
}

fn main() {
    let mut value = 5;
    println!("Counting: {}", add_1(&mut value).call());
    println!("Counting: {}", add_1(&mut value).call());
    assert_eq!(value, 7);
}
```

It can also be used on member functions, as such:
```
use desaturate::{desaturate, Blocking};

struct MyType<'a> {
    inner: i32,
    pointer: &'a i32,
}

impl<'a> MyType<'a> {
    #[desaturate(lifetime = "'a")]
    async fn get_inner_mut(&mut self) -> &mut i32 {
        &mut self.inner
    }
    #[desaturate]
    async fn get_pointer(&self) -> &'a i32 {
        self.pointer
    }
}
"#
)]

use core::future::{Future, IntoFuture};

#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
#[cfg(feature = "std")]
pub mod boxed;

#[macro_use]
mod macros;
use macros::*;

#[cfg(feature = "macros")]
mod macro_documentation;
#[cfg_attr(docsrs, doc(cfg(feature = "macros")))]
#[cfg(feature = "macros")]
pub use macro_documentation::*;

#[must_use]
pub trait Blocking<Output: Sized> {
    fn call(self) -> Output;
}
impl<Output, T: FnOnce() -> Output> Blocking<Output> for T {
    fn call(self) -> Output {
        (self)()
    }
}

mod private {
    pub trait Sealed<Output> {}
}

features! {async fn: create_asyncable!{ T => Blocking<T> + IntoFuture<Output = T> }}

features! {!async fn: create_asyncable!{ T => Blocking<T> }}

features! {async !fn: create_asyncable!{ T => IntoFuture<Output = T> }}

features! {!async !fn: create_asyncable!{ T => }}

features! {!async !fn:
    impl<O> Desaturated<O> for () {}
    impl<O> private::Sealed<O> for () {}
}

#[doc(hidden)]
pub trait AsyncFnOnce<'a, Args: 'a, Out> {
    type Output: Future<Output = Out> + 'a;
    fn call(self, args: Args) -> Self::Output;
}

impl<'a, Args: 'a, Out, Fun, Fut> AsyncFnOnce<'a, Args, Out> for Fun
where
    Fun: FnOnce(Args) -> Fut,
    Fut: Future<Output = Out> + 'a,
{
    type Output = Fut;
    fn call(self, args: Args) -> Self::Output {
        (self)(args)
    }
}

pub trait IntoDesaturatedWith<'a, Args: 'a, Output>: AsyncFnOnce<'a, Args, Output> + 'a {
    fn desaturate_with(
        self,
        args: Args,
        fun: impl 'a + FnOnce(Args) -> Output,
    ) -> impl Desaturated<Output> + 'a;
}

impl<'a, O: 'a, A: 'a, F: 'a + AsyncFnOnce<'a, A, O>> IntoDesaturatedWith<'a, A, O> for F {
    features! {async fn:
        #[inline(always)]
        fn desaturate_with(self, args: A, fun: impl FnOnce(A) -> O + 'a) -> impl Desaturated<O> + 'a {
            struct Holder<'a, Output, Args: 'a, NormalFunc: FnOnce(Args) -> Output, AsyncFunc: AsyncFnOnce<'a, Args, Output>> {
                args: Args,
                fun: NormalFunc,
                fut: AsyncFunc,
                phantom: core::marker::PhantomData<&'a ()>,
            }
            impl<'a, Output, Args: 'a, NormalFunc: FnOnce(Args) -> Output, AsyncFunc: AsyncFnOnce<'a, Args, Output>> IntoFuture for Holder<'a, Output, Args, NormalFunc, AsyncFunc> {
                type Output = Output;

                type IntoFuture = AsyncFunc::Output;

                #[inline(always)]
                fn into_future(self) -> Self::IntoFuture {
                    self.fut.call(self.args)
                }
            }
            impl<'a, Output, Args: 'a, NormalFunc: FnOnce(Args) -> Output, AsyncFunc: AsyncFnOnce<'a, Args, Output>> Blocking<Output> for Holder<'a, Output, Args, NormalFunc, AsyncFunc> {
                #[inline(always)]
                fn call(self) -> Output {
                    (self.fun)(self.args)
                }
            }
            impl<'a, Output, Args: 'a, NormalFunc: FnOnce(Args) -> Output, AsyncFunc: AsyncFnOnce<'a, Args, Output>> private::Sealed<Output> for Holder<'a, Output, Args, NormalFunc, AsyncFunc> {}
            Holder {
                args,
                fun,
                fut: self,
                phantom: Default::default(),
            }
        }
    }
    features! {async !fn:
        #[inline(always)]
        fn desaturate_with(self, args: A, _: impl FnOnce(A) -> O) -> impl Desaturated<O> + 'a {
            struct Holder<Out, T: Future<Output = Out>>(T);
            impl<Out, T: Future<Output = Out>> private::Sealed<Out> for Holder<Out, T> {}
            impl<Out, T: Future<Output = Out>> IntoFuture for Holder<Out, T> {
                type Output = Out;

                type IntoFuture = T;

                #[inline(always)]
                fn into_future(self) -> T {
                    self.0
                }
            }
            Holder(self.call(args))
        }
    }
    features! {!async fn:
        #[inline(always)]
        fn desaturate_with(self, args: A, fun: impl FnOnce(A) -> O + 'a) -> impl Desaturated<O> + 'a {
            struct Holder<'a, Output, Args: 'a, Function: FnOnce(Args) -> Output> {
                args: Args,
                fun: Function,
                phantom: core::marker::PhantomData<&'a ()>,
            }
            impl<'a, Output, Args: 'a, Function: FnOnce(Args) -> Output> private::Sealed<Output> for Holder<'a, Output, Args, Function> {}
            impl<'a, Output, Args: 'a, Function: FnOnce(Args) -> Output> Blocking<Output> for Holder<'a, Output, Args, Function> {
                #[inline(always)]
                fn call(self) -> Output {
                    (self.fun)(self.args)
                }
            }
            Holder {
                args,
                fun,
                phantom: Default::default(),
            }
        }
    }
    features! {!async !fn:
        #[inline(always)]
        fn desaturate_with(self, _: A, _: impl FnOnce(A) -> O) -> impl Desaturated<O> +'a {
            ()
        }
    }
}

pub trait IntoDesaturated<'a, Output>: IntoFuture<Output = Output> + 'a {
    fn desaturate(self, fun: impl FnOnce() -> Output + 'a) -> impl Desaturated<Output> + 'a;
}

impl<'a, Output: 'a, F: Future<Output = Output> + 'a> IntoDesaturated<'a, Output> for F {
    #[inline(always)]
    fn desaturate(self, fun: impl FnOnce() -> Output + 'a) -> impl Desaturated<Output> + 'a {
        (|()| self).desaturate_with((), |()| fun())
    }
}

#[cfg(test)]
mod tests {
    #[allow(unused_imports)]
    use super::{Blocking, Desaturated, IntoDesaturated, IntoDesaturatedWith};
    #[allow(unused_imports)]
    use core::sync::atomic::{AtomicBool, Ordering};
    #[test]
    #[cfg_attr(not(feature = "blocking"), ignore)]
    fn normal_returns_right() {
        #[cfg(feature = "blocking")]
        {
            let async_executed = AtomicBool::new(false);
            let normal_executed = AtomicBool::new(false);
            let async_stuff = async {
                async_executed.store(true, Ordering::Relaxed);
                5
            };
            let sync_stuff = || {
                normal_executed.store(true, Ordering::Relaxed);
                5
            };
            let desaturated = async_stuff.desaturate(sync_stuff);
            assert_eq!(5, desaturated.call());
            assert_eq!(false, async_executed.load(Ordering::Relaxed));
            assert_eq!(true, normal_executed.load(Ordering::Relaxed));
        }
    }
    #[tokio::test]
    #[cfg_attr(not(feature = "async"), ignore)]
    async fn async_returns_right() {
        #[cfg(feature = "async")]
        {
            let async_executed = AtomicBool::new(false);
            let normal_executed = AtomicBool::new(false);
            let async_stuff = async {
                async_executed.store(true, Ordering::Relaxed);
                5
            };
            let sync_stuff = || {
                normal_executed.store(true, Ordering::Relaxed);
                5
            };
            let desaturated = async_stuff.desaturate(sync_stuff);
            assert_eq!(5, desaturated.await);
            assert_eq!(true, async_executed.load(Ordering::Relaxed));
            assert_eq!(false, normal_executed.load(Ordering::Relaxed));
        }
    }
    fn do_stuff(with: &i32) -> impl Desaturated<i32> + '_ {
        let async_stuff = async { *with * 2 };
        let sync_stuff = || *with * 2;
        async_stuff.desaturate(sync_stuff)
    }
    fn do_stuff_with_pointer<'a>(with: &'a i32) -> impl Desaturated<i32> + '_ {
        let async_stuff = |var: &'a i32| async move { *var * 2 };
        let sync_stuff = |var: &'a i32| *var * 2;
        async_stuff.desaturate_with(with, sync_stuff)
    }
    fn do_stuff_with_pointer_in_inner_function<'a>(with: &'a i32) -> impl Desaturated<i32> + '_ {
        async fn async_stuff(var: &i32) -> i32 {
            *var * 2
        }
        fn sync_stuff(var: &i32) -> i32 {
            *var * 2
        }
        async_stuff.desaturate_with(with, sync_stuff)
    }
    #[test]
    #[cfg_attr(not(feature = "blocking"), ignore)]
    fn can_take_pointer() {
        #[cfg(feature = "blocking")]
        {
            assert_eq!(20, do_stuff(&10).call());
            let arg = 30;
            assert_eq!(60, do_stuff_with_pointer(&arg).call());
            assert_eq!(60, do_stuff_with_pointer_in_inner_function(&arg).call());
        }
    }
    #[tokio::test]
    #[cfg_attr(not(feature = "async"), ignore)]
    async fn async_can_take_pointer() {
        #[cfg(feature = "async")]
        {
            assert_eq!(20, do_stuff(&10).await);
            let arg = 30;
            assert_eq!(60, do_stuff_with_pointer(&arg).await);
            assert_eq!(60, do_stuff_with_pointer_in_inner_function(&arg).await);
        }
    }
    #[test]
    #[cfg_attr(any(feature = "async", feature = "blocking"), ignore)]
    fn it_always_builds() {
        _ = do_stuff(&10)
    }
}
