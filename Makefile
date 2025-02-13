.PHONY: all
all:
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	$(MAKE) build

.PHONY: build
build:
	dune build && ln -sf _build/default/bin/main.exe scylla

test: all test-chacha

test-%: out/%.rs
	cd out/$* && cargo build && target/debug/$*

# FIXME: we can't write out/%/src/%.rs in GNU Make (which is what is now being generated)
.PRECIOUS: out/%.rs

out/%.rs: test/%.c $(wildcard test/include/*)
	./scylla $< --output out/$*/src/
	for f in rs/*.rs; do cp $$f out/$*/src/; done
