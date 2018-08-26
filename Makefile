build:
	cargo build

loop:
	-@cargo build
	-@echo Waiting for changes...
	-@fswatch -o src Cargo.toml tests | while read f; do cargo build; echo Waiting for changes...; done

opt:
	cargo build --release

run:
	cargo run

test:
	RUST_BACKTRACE=1 cargo test -- --nocapture

test-loop:
	-@make test
	-@echo Waiting for changes...
	-@fswatch -o src Cargo.toml tests | while read f; do make test; echo Waiting for changes...; done
