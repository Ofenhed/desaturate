use core::future::{Future, IntoFuture};
#[cfg(feature = "macros")]
pub use desaturate_macros::*;

#[must_use]
pub trait Blocking<Output: Sized> {
    fn call(self) -> Output;
}

mod internal {
    pub trait OnlyAutomatic<Output> {}
}

macro_rules! features {
    (: $($rest:tt)+) => {
        $($rest)+
    };
    (async $($rest:tt)+) => {
        #[cfg(feature = "generate-async")]
        features!{ $($rest)+ }
    };
    (!async $($rest:tt)+) => {
        #[cfg(not(feature = "generate-async"))]
        features!{ $($rest)+ }
    };
    (fn $($rest:tt)+) => {
        #[cfg(feature = "generate-blocking")]
        features!{ $($rest)+ }
    };
    (!fn $($rest:tt)+) => {
        #[cfg(not(feature = "generate-blocking"))]
        features!{ $($rest)+ }
    };
}

macro_rules! create_asyncable {
    ($T:ident => $($($traits:tt)+)?) => {
        /// This function can be used either asynchronously (with `await`) or synchronously with [`Blocking::call`].
        #[must_use]
        pub trait Desaturated<'a, $T>: $($($traits)+ +)? 'a + internal::OnlyAutomatic<$T> {}
        $(
            impl<'a, $T, U: $($traits)+ + 'a> Desaturated<'a, $T> for U {}
            impl<$T, U: $($traits)+> internal::OnlyAutomatic<$T> for U {}
        )?
    };
}

features! {async fn: create_asyncable!{ T => Blocking<T> + IntoFuture<Output = T> }}

features! {!async fn: create_asyncable!{ T => Blocking<T> }}

features! {async !fn: create_asyncable!{ T => IntoFuture<Output = T> }}

features! {!async !fn: create_asyncable!{ T => }}

features! {!async !fn:
    impl<'a, O> Desaturated<'a, O> for () {}
    impl<O> internal::OnlyAutomatic<O> for () {}
}

pub trait IntoDesaturatedWith<'fut, 'args: 'fut, Args, Output, Fut>
where
    Args: 'args,
    Fut: 'fut + Future<Output = Output>,
    Self: 'fut + FnOnce(Args) -> Fut,
{
    fn desaturate_with(
        self,
        args: Args,
        fun: impl FnOnce(Args) -> Output + 'fut,
    ) -> impl Desaturated<'fut, Output>;
}

impl<
        'fut,
        'args: 'fut,
        O: 'fut,
        A: 'args,
        F: Future<Output = O> + 'fut,
        AF: 'fut + FnOnce(A) -> F,
    > IntoDesaturatedWith<'fut, 'args, A, O, F> for AF
{
    features! {async fn:
        #[inline(always)]
        fn desaturate_with(self, args: A, fun: impl 'fut + FnOnce(A) -> O) -> impl Desaturated<'fut, O> {
            struct Holder<Output, Args, NormalFunc: FnOnce(Args) -> Output, Fut: Future<Output = Output>, AsyncFunc: FnOnce(Args) -> Fut> {
                args: Args,
                fun: NormalFunc,
                fut: AsyncFunc,
            }
            impl<Output, Args, NormalFunc: FnOnce(Args) -> Output, Fut: Future<Output = Output>, AsyncFunc: FnOnce(Args) -> Fut> IntoFuture for Holder<Output, Args, NormalFunc, Fut, AsyncFunc> {
                type Output = Output;

                type IntoFuture = Fut;

                #[inline(always)]
                fn into_future(self) -> Self::IntoFuture {
                    (self.fut)(self.args)
                }
            }
            impl<Output, Args, NormalFunc: FnOnce(Args) -> Output, Fut: Future<Output = Output>, AsyncFunc: FnOnce(Args) -> Fut> Blocking<Output> for Holder<Output, Args, NormalFunc, Fut, AsyncFunc> {
                #[inline(always)]
                fn call(self) -> Output {
                    (self.fun)(self.args)
                }
            }
            Holder {
                args,
                fun,
                fut: self
            }
        }
    }
    features! {async !fn:
        #[inline(always)]
        fn desaturate_with(self, args: A, _: impl FnOnce(A) -> O) -> impl Desaturated<'fut, O> {
            self(args)
        }
    }
    features! {!async fn:
        #[inline(always)]
        fn desaturate_with(self, args: A, fun: impl FnOnce(A) -> O) -> impl Desaturated<'fut, O> {
            struct Holder<'args, 'fut, Output, Args: 'args, Function: 'fut + FnOnce(Args) -> Output> {
                args: Args,
                fun: Function,
            }
            impl<'args, 'fut, Output, Args: 'args, Function: 'fut + FnOnce(Args) -> Output> Blocking<Output> for Holder<'args, 'fut, Output, Args, Function> {
                #[inline(always)]
                fn call(self) -> Output {
                    (self.fun)(self.args)
                }
            }
            Holder {
                args,
                fun,
            }
        }
    }
    features! {!async !fn:
        #[inline(always)]
        fn desaturate_with(self, _: A, _: impl FnOnce(A) -> O) -> impl Desaturated<'fut, O> {
            ()
        }
    }
}

pub trait IntoDesaturated<'a, Output>: IntoFuture<Output = Output> + 'a {
    fn desaturate(self, fun: impl FnOnce() -> Output + 'a) -> impl Desaturated<'a, Output>;
}

impl<'a, Output: 'a, F: Future<Output = Output> + 'a> IntoDesaturated<'a, Output> for F {
    #[inline(always)]
    fn desaturate(self, fun: impl FnOnce() -> Output + 'a) -> impl Desaturated<'a, Output> {
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
    #[cfg_attr(not(feature = "generate-blocking"), ignore)]
    fn normal_returns_right() {
        #[cfg(feature = "generate-blocking")]
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
    #[cfg_attr(not(feature = "generate-async"), ignore)]
    async fn async_returns_right() {
        #[cfg(feature = "generate-async")]
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
    // TODO: Figure out how to get this to work
    //fn do_stuff_with(with: &i32) -> impl Desaturated<'_, i32> + '_ {
    //    let async_stuff = |var: &i32| async move {
    //        *var * 2
    //    };
    //    let sync_stuff = |var: &i32| {
    //        *var * 2
    //    };
    //    async_stuff.desaturate_with(with, sync_stuff)
    //}
    #[test]
    #[cfg_attr(not(feature = "generate-blocking"), ignore)]
    fn can_take_pointer() {
        #[cfg(feature = "generate-blocking")]
        assert_eq!(20, do_stuff(&10).call())
    }
    #[tokio::test]
    #[cfg_attr(not(feature = "generate-async"), ignore)]
    async fn async_can_take_pointer() {
        #[cfg(feature = "generate-async")]
        assert_eq!(20, do_stuff(&10).await)
    }
    #[test]
    #[cfg_attr(any(feature = "generate-async", feature = "generate-blocking"), ignore)]
    fn it_always_builds() {
        _ = do_stuff(&10)
    }
}
