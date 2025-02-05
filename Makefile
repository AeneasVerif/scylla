.PHONY: all
all:
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	$(MAKE) build

.PHONY: build
build:
	dune build && ln -sf _build/default/bin/main.exe scylla

test: test-chacha

test-%: out/%.rs
	@true

out/%.rs: test/%.c $(wildcard test/include/*)
	./scylla $< --output out
