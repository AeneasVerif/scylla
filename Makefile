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
all: build

lib/DataModel.ml: misc/data_model.exe
	$< > $@

misc/data_model.exe: misc/data_model.o
	$(CC) $< -o $@

.PHONY: build
build: lib/DataModel.ml
	@ocamlfind list | grep -q krml || test -L lib/krml || echo "⚠️⚠️⚠️ krml not found; we suggest cd lib && ln -s path/to/karamel/lib krml"
	dune build && ln -sf _build/default/bin/main.exe scylla

# This target is removed in CI because CI builds `scylla` itself.
scylla: build

.PHONY: test
test: regen-outputs
	cd out/hacl && cargo test

# We extract all of the tests into the same hacl directory
.PHONY: regen-outputs
regen-outputs: test-chacha test-bignum_base
	for f in rs/*.rs; do cp $$f out/hacl/src/; done

.PHONY: test-%
test-%: test/%.c $(wildcard test/include/*) scylla
	./scylla $(SCYLLA_OPTS) $< --output out/hacl/src/
