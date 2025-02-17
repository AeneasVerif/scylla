# We try to figure out the best include paths, compiler options, etc. from the build system.

SCYLLA_OPTS = --ccopts -DKRML_UNROLL_MAX=0,-I,test/include

# On OSX, querying xcrun appears to provide the sysroot.
ifeq ($(shell uname -s),Darwin)
  ISYSROOT=$(shell xcrun --show-sdk-path)
  ifneq ($(ISYSROOT),)
    SCYLLA_OPTS += --ccopts -I,$(ISYSROOT)/usr/include
  endif
endif


.PHONY: all
all:
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	$(MAKE) build

.PHONY: build
build:
	dune build && ln -sf _build/default/bin/main.exe scylla

# We extract all of the tests into the same hacl directory
test: test-chacha test-bignum_base
	for f in rs/*.rs; do cp $$f out/hacl/src/; done
	cd out/hacl && cargo build && target/debug/hacl

test-%: test/%.c $(wildcard test/include/*) | all
	./scylla $(SCYLLA_OPTS) $< --output out/hacl/src/
