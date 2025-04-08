# We try to figure out the best include paths, compiler options, etc. from the build system.

SCYLLA_OPTS = --ccopts -DKRML_UNROLL_MAX=0,-I,test/include,-I,test/ --errors_as_warnings

# On OSX, querying xcrun appears to provide the sysroot.
# FIXME: no, you want the sysroot from the clang version that clangml was compiled with
ifeq ($(shell uname -s),Darwin)
  ISYSROOT=$(shell xcrun --show-sdk-path)
  ifneq ($(ISYSROOT),)
    SCYLLA_OPTS += --ccopts -I,$(ISYSROOT)/usr/include
  endif
endif


.PHONY: all
all: build format-check

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
regen-outputs: test-chacha test-bignum_base test-bignum
	for f in rs/*.rs; do cp $$f out/hacl/src/; done

test-bignum:
	./scylla $(SCYLLA_OPTS) test/internal/Hacl_Bignum_Base.h test/Hacl_Bignum.c test/Hacl_Bignum4096.c --output out/hacl/src/

.PHONY: test-%
test-%: test/%.c $(wildcard test/include/*) scylla
	./scylla $(SCYLLA_OPTS) $< --output out/hacl/src/

.PHONY: nix-magic
nix-magic:
	nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes

.PHONY: format-check
format-check:
	@if ! dune build @fmt >/dev/null 2>&1; then \echo "\033[0;31m⚠️⚠️⚠️ SUGGESTED: $(MAKE) format-apply\033[0;m"; fi

.PHONY: format-apply
format-apply:
	dune fmt >/dev/null || true
