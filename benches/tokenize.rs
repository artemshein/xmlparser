use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};

fn tokenize_fork(input: &str) -> usize {
    xmlparser::Tokenizer::from(input)
        .filter_map(|t| t.ok())
        .count()
}

fn tokenize_upstream(input: &str) -> usize {
    xmlparser_upstream::Tokenizer::from(input)
        .filter_map(|t| t.ok())
        .count()
}

fn collect_fork(input: &str) -> Vec<xmlparser::Token> {
    xmlparser::Tokenizer::from(input)
        .filter_map(|t| t.ok())
        .collect()
}

fn collect_upstream<'a>(input: &'a str) -> Vec<xmlparser_upstream::Token<'a>> {
    xmlparser_upstream::Tokenizer::from(input)
        .filter_map(|t| t.ok())
        .collect()
}

fn load(name: &str) -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("benches/data")
        .join(name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read {}: {}", path.display(), e))
}

fn try_load(name: &str) -> Option<String> {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("benches/data")
        .join(name);
    std::fs::read_to_string(&path).ok()
}

fn bench_tokenize(c: &mut Criterion) {
    let dense = load("dense.xml");
    let spaced = load("spaced.xml");
    let dtd = load("dtd.xml");
    let large = try_load("large.xml");

    let mut group = c.benchmark_group("tokenize");

    let mut fixtures: Vec<(&str, &str)> = vec![
        ("dense", dense.as_str()),
        ("spaced", spaced.as_str()),
        ("dtd", dtd.as_str()),
    ];
    if let Some(ref s) = large {
        fixtures.push(("large", s.as_str()));
    }

    for (name, input) in fixtures {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("fork", name), input, |b, i| {
            b.iter(|| tokenize_fork(std::hint::black_box(i)))
        });
        group.bench_with_input(BenchmarkId::new("upstream", name), input, |b, i| {
            b.iter(|| tokenize_upstream(std::hint::black_box(i)))
        });
    }

    group.finish();
}

fn bench_collect(c: &mut Criterion) {
    let dense = load("dense.xml");
    let spaced = load("spaced.xml");
    let dtd = load("dtd.xml");

    let mut group = c.benchmark_group("collect");

    for (name, input) in [
        ("dense", dense.as_str()),
        ("spaced", spaced.as_str()),
        ("dtd", dtd.as_str()),
    ] {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("fork", name), input, |b, i| {
            b.iter(|| collect_fork(std::hint::black_box(i)))
        });
        group.bench_with_input(BenchmarkId::new("upstream", name), input, |b, i| {
            b.iter(|| collect_upstream(std::hint::black_box(i)))
        });
    }

    group.finish();
}

fn bench_token_sizes(_c: &mut Criterion) {
    let fork_token = std::mem::size_of::<xmlparser::Token>();
    let upstream_token = std::mem::size_of::<xmlparser_upstream::Token<'_>>();

    let dense = load("dense.xml");
    let fork_tokens: Vec<_> = xmlparser::Tokenizer::from(dense.as_str())
        .filter_map(|t| t.ok())
        .collect();
    let upstream_tokens: Vec<_> = xmlparser_upstream::Tokenizer::from(dense.as_str())
        .filter_map(|t| t.ok())
        .collect();

    let n = fork_tokens.len();
    assert_eq!(n, upstream_tokens.len());

    println!(
        "\n=== Token memory comparison (dense.xml, {} tokens) ===",
        n
    );
    println!(
        "  fork     Token size: {:>3} bytes  →  Vec<Token> heap: {} bytes",
        fork_token,
        n * fork_token
    );
    println!(
        "  upstream Token size: {:>3} bytes  →  Vec<Token> heap: {} bytes",
        upstream_token,
        n * upstream_token
    );
    println!(
        "  savings per token:    {:>3} bytes  →  total savings:   {} bytes ({:.1}%)",
        upstream_token.saturating_sub(fork_token),
        n * upstream_token.saturating_sub(fork_token),
        100.0 * upstream_token.saturating_sub(fork_token) as f64 / upstream_token as f64
    );
}

criterion_group!(benches, bench_tokenize, bench_collect, bench_token_sizes);
criterion_main!(benches);
