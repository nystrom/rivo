build: .FORCE
	@cargo build

loop: .FORCE
	@./loop make build

opt:
	@cargo build --release

run:
	@cargo run

test:
	@RUST_BACKTRACE=1 cargo test -- --nocapture

test-loop:
	@./loop make test

.FORCE:
