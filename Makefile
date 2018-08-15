build:
	cargo build

loop:
	-@cargo build
	-@fswatch -o src Cargo.toml tests | while read f; do cargo test; done

opt:
	cargo build --release

run:
	cargo run

test:
	cargo test -- --nocapture

test-loop:
	-@cargo test -- --nocapture
	-@fswatch -o src Cargo.toml tests | while read f; do cargo test -- --nocapture; done
