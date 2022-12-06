#[macro_export]
macro_rules! benchmark {
    ($($token:tt)+) => {
        {
            let start = std::time::Instant::now();
            let res = $($token)+;
            println!("Elapsed {:?}", start.elapsed());
            res
        }
    }
}
