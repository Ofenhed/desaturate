#[cfg(feature = "macros")]
pub use desaturate_macros::*;
use core::future::IntoFuture;

#[must_use]
pub trait Syncable<T: Sized> {
    fn call(self) -> T;
}

/// This function can be used either asynchronously (with `await`) or synchronously with [`Syncable::call`].
#[must_use]
pub trait Asyncable<T>: IntoFuture<Output = T> + Syncable<T> {}

impl<O, T: IntoFuture<Output = O> + Syncable<O>> Asyncable<O> for T {}

impl<Output, T: Into<Output> + IntoFuture<Output = Output>> Syncable<Output> for T {
    #[inline(always)]
    fn call(self) -> Output {
        self.into()
    }
}
