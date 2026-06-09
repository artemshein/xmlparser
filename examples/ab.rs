// Interleaved A/B throughput harness. Times the fork and the upstream crate in
// alternating rounds so machine drift affects both equally; reports the median
// fork/upstream ratio, which is far more stable run-to-run than criterion's
// absolute numbers. Lower ratio = fork faster relative to upstream.
//
// Usage: cargo run --release --example ab [fixture.xml] [iters_per_round] [rounds]

use std::hint::black_box;
use std::time::Instant;

fn count_fork(input: &str) -> usize {
    xmlparser::Tokenizer::from(input).filter_map(|t| t.ok()).count()
}

fn count_up(input: &str) -> usize {
    xmlparser_upstream::Tokenizer::from(input).filter_map(|t| t.ok()).count()
}

fn main() {
    let mut args = std::env::args().skip(1);
    let fixture = args.next().unwrap_or_else(|| "dense.xml".to_string());
    let iters: usize = args.next().and_then(|s| s.parse().ok()).unwrap_or(2000);
    let rounds: usize = args.next().and_then(|s| s.parse().ok()).unwrap_or(61);

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("benches/data")
        .join(&fixture);
    let data = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read {}: {}", path.display(), e));

    // Sanity + warmup. Keep warmup light for huge inputs.
    assert_eq!(count_fork(&data), count_up(&data));
    let warmup = if data.len() > 10 * 1024 * 1024 {
        2
    } else {
        iters.max(1) / 4 + 50
    };
    for _ in 0..warmup {
        black_box(count_fork(&data));
        black_box(count_up(&data));
    }

    let mut fork = Vec::with_capacity(rounds);
    let mut up = Vec::with_capacity(rounds);
    for _ in 0..rounds {
        let t = Instant::now();
        for _ in 0..iters {
            black_box(count_fork(black_box(&data)));
        }
        fork.push(t.elapsed().as_nanos());

        let t = Instant::now();
        for _ in 0..iters {
            black_box(count_up(black_box(&data)));
        }
        up.push(t.elapsed().as_nanos());
    }

    fork.sort_unstable();
    up.sort_unstable();
    let fmed = fork[rounds / 2] as f64 / iters as f64;
    let umed = up[rounds / 2] as f64 / iters as f64;
    let mib = data.len() as f64 / (1024.0 * 1024.0);
    let to_thrpt = |ns_per_iter: f64| mib / (ns_per_iter / 1e9);

    println!("fixture: {} ({} bytes)", fixture, data.len());
    println!("rounds: {}, iters/round: {}", rounds, iters);
    println!(
        "fork     median: {:>9.0} ns/iter  ({:>7.1} MiB/s)",
        fmed,
        to_thrpt(fmed)
    );
    println!(
        "upstream median: {:>9.0} ns/iter  ({:>7.1} MiB/s)",
        umed,
        to_thrpt(umed)
    );
    println!(
        "ratio fork/upstream: {:.4}  (lower = fork faster; fork is {:.1}% faster)",
        fmed / umed,
        (umed / fmed - 1.0) * 100.0
    );
}
