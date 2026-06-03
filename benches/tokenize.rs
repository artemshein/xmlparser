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

// Dense XML: minimal whitespace, mostly element data.
static DENSE: &str = include_str!("data/dense.xml");

// Spaced XML: heavy inter-element whitespace — stresses the skip-whitespace path
// that previously used recursion.
static SPACED: &str = include_str!("data/spaced.xml");

// DTD XML: document with a DOCTYPE block containing multiple ENTITY declarations,
// comments and ignored sections — stresses the Dtd/AfterDeclaration skip paths.
static DTD: &str = include_str!("data/dtd.xml");

fn bench_tokenize(c: &mut Criterion) {
    let mut group = c.benchmark_group("tokenize");

    for (name, input) in [("dense", DENSE), ("spaced", SPACED), ("dtd", DTD)] {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("fork", name),
            input,
            |b, i| b.iter(|| tokenize_fork(criterion::black_box(i))),
        );
        group.bench_with_input(
            BenchmarkId::new("upstream", name),
            input,
            |b, i| b.iter(|| tokenize_upstream(criterion::black_box(i))),
        );
    }

    group.finish();
}

fn bench_collect(c: &mut Criterion) {
    let mut group = c.benchmark_group("collect");

    for (name, input) in [("dense", DENSE), ("spaced", SPACED), ("dtd", DTD)] {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("fork", name),
            input,
            |b, i| b.iter(|| collect_fork(criterion::black_box(i))),
        );
        group.bench_with_input(
            BenchmarkId::new("upstream", name),
            input,
            |b, i| b.iter(|| collect_upstream(criterion::black_box(i))),
        );
    }

    group.finish();
}

fn bench_token_sizes(_c: &mut Criterion) {
    let fork_token = std::mem::size_of::<xmlparser::Token>();
    let upstream_token = std::mem::size_of::<xmlparser_upstream::Token<'_>>();

    // Count tokens and compute Vec memory footprint for dense XML.
    let fork_tokens: Vec<_> = xmlparser::Tokenizer::from(DENSE)
        .filter_map(|t| t.ok())
        .collect();
    let upstream_tokens: Vec<_> = xmlparser_upstream::Tokenizer::from(DENSE)
        .filter_map(|t| t.ok())
        .collect();

    let n = fork_tokens.len();
    assert_eq!(n, upstream_tokens.len());

    println!("\n=== Token memory comparison (dense.xml, {} tokens) ===", n);
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
