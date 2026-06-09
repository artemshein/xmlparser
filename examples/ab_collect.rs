// Interleaved A/B harness for the collect path (Vec<Token> allocation included).
use std::hint::black_box;
use std::time::Instant;

fn main() {
    let mut args = std::env::args().skip(1);
    let fixture = args.next().unwrap_or_else(|| "dense.xml".to_string());
    let iters: usize = args.next().and_then(|s| s.parse().ok()).unwrap_or(1000);
    let rounds: usize = args.next().and_then(|s| s.parse().ok()).unwrap_or(21);

    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("benches/data")
        .join(&fixture);
    let data = std::fs::read_to_string(&path).unwrap();

    for _ in 0..iters / 4 + 20 {
        black_box(xmlparser::Tokenizer::from(data.as_str()).filter_map(|t| t.ok()).collect::<Vec<_>>());
        black_box(xmlparser_upstream::Tokenizer::from(data.as_str()).filter_map(|t| t.ok()).collect::<Vec<_>>());
    }

    let mut fork = Vec::new();
    let mut up = Vec::new();
    for _ in 0..rounds {
        let t = Instant::now();
        for _ in 0..iters {
            black_box(xmlparser::Tokenizer::from(black_box(data.as_str())).filter_map(|t| t.ok()).collect::<Vec<_>>());
        }
        fork.push(t.elapsed().as_nanos());
        let t = Instant::now();
        for _ in 0..iters {
            black_box(xmlparser_upstream::Tokenizer::from(black_box(data.as_str())).filter_map(|t| t.ok()).collect::<Vec<_>>());
        }
        up.push(t.elapsed().as_nanos());
    }
    fork.sort_unstable();
    up.sort_unstable();
    let fmed = fork[rounds / 2] as f64 / iters as f64;
    let umed = up[rounds / 2] as f64 / iters as f64;
    let mib = data.len() as f64 / (1024.0 * 1024.0);
    println!(
        "{}: fork {:.1} MiB/s, upstream {:.1} MiB/s, ratio {:.4}",
        fixture, mib / (fmed / 1e9), mib / (umed / 1e9), fmed / umed
    );
}
