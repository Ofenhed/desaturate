macro_rules! features {
    (: $($rest:tt)+) => {
        $($rest)+
    };
    (async $($rest:tt)+) => {
        #[cfg(feature = "async")]
        features!{ $($rest)+ }
    };
    (!async $($rest:tt)+) => {
        #[cfg(not(feature = "async"))]
        features!{ $($rest)+ }
    };
    (fn $($rest:tt)+) => {
        #[cfg(feature = "blocking")]
        features!{ $($rest)+ }
    };
    (!fn $($rest:tt)+) => {
        #[cfg(not(feature = "blocking"))]
        features!{ $($rest)+ }
    };
}

macro_rules! create_asyncable {
    ($T:ident => $($($traits:tt)+)?) => {
        /// This trait is dynamic based on feature flags. For this reason, it's not allowed to be
        /// directly implemented, as this would break the [additive features
        /// guideline](https://doc.rust-lang.org/cargo/reference/features.html#feature-unification).
        ///
        /// When the flag `async` is set, this trait implements [`IntoFuture`].
        ///
        /// When the flag `blocking` is set, this trait implements [`Blocking`].
        ///
        /// Note that the [`into_future()`] (which is implicitly called when you use `.await`) and
        /// [`call()`] functions both consume this value, and the arguments. For this reason, your
        /// arguments can include owned variables, as they can only be consumed once.
        ///
        /// [`IntoFuture`]: core::future::IntoFuture
        /// [`into_future()`]: core::future::IntoFuture::into_future
        /// [`call()`]: Blocking::call
        pub trait Desaturated<$T>: $($($traits)+ +)? private::Sealed<$T> {}
        $(
            impl<$T, U: $($traits)+ + private::Sealed<$T>> Desaturated<$T> for U {}
        )?
    };
}

pub(crate) use {create_asyncable, features};
