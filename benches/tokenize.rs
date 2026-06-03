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

criterion_group!(benches, bench_tokenize);
criterion_main!(benches);
