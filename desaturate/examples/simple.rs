use desaturate::{desaturate, Blocking};

#[desaturate]
async fn another_function(i: i32) -> i32 {
    i
}

struct Tester {
    inner: i32,
}

impl Tester {
    #[desaturate]
    async fn add_inner<'a>(&'a mut self, i: &i32) -> &'a i32 {
        self.inner = self.inner + another_function(*i).await;
        &self.inner
        //5 * another_function(*i).call()
    }

    #[desaturate]
    async fn test_more(&self, (i, j, k): (&i32, u16, &i32)) -> i32 {
        *i + Self::test(&(*i + i32::from(j) + *k + self.inner)).await
    }

    #[desaturate]
    async fn test(i: &i32) -> i32 {
        5 * another_function(*i).await
    }
}

#[tokio::main]
async fn main() {
    #[cfg(all(feature = "generate-async", feature = "generate-blocking"))]
    {
        let arg = 3;
        let arg2 = 0;
        let mut tester = Tester { inner: 0 };
        let result1 = tester.test_more((&arg, 7, &arg2)).await;
        let result2 = tester.test_more((&arg, 7, &arg2)).call();
        let from_pointer = *tester.add_inner(&arg).await;
        let from_pointer2 = *tester.add_inner(&arg).call();
        assert_eq!(from_pointer + 10, from_pointer2);
        assert_eq!(result1, result2);
    }
}
