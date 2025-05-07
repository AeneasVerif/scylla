# We need the sysroot for the version of llvm clangml was built against. Running brew takes ages so
# we cache the result. No assumptions made on the underlying shell or gnu-ness of sed.
ifeq ($(shell uname -s),Darwin)
  ISYSROOT=$(shell [ -f .sysroot ] && cat .sysroot || \
    brew info llvm@15 | egrep '^/' | sed 's!\(15\.[^ ]*\) .*!\1/Toolchains/LLVM\1.xctoolchain!' | tee .sysroot)
  ifneq ($(ISYSROOT),)
    SCYLLA_SYSROOT_OPT = --ccopts -isysroot,$(ISYSROOT)/
  endif
endif

# We try to figure out the best include paths, compiler options, etc. from the build system.
SCYLLA_OPTS += --ccopts -DKRML_UNROLL_MAX=0,-I,test/hacl/include,-I,test/hacl/ --errors_as_warnings $(SCYLLA_SYSROOT_OPT) --ignore_lib_errors

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

scylla: build

.PHONY: test
test: regen-outputs test-symcrypt
	cd out/hacl && cargo test

# Approximation -- but we are not doing the whole gcc -MM dance
SYMCRYPT_SOURCES=$(wildcard $(addprefix $(SYMCRYPT_HOME)/,inc/symcrypt_internal.h lib/sha3.c lib/sha3_*.c lib/shake.c))
SYMCRYPT_DYLIB=$(wildcard $(addprefix $(SYMCRYPT_HOME)/build/module/generic/libsymcrypt.,dll so dylib))

test-symcrypt: $(SYMCRYPT_HOME)/rs/src/sha3.rs
	cd $(SYMCRYPT_HOME)/rs && \
	  DYLD_LIBRARY_PATH=$(dir $(SYMCRYPT_DYLIB)):$$DYLD_LIBRARYPATH \
	  LD_LIBRARY_PATH=$(dir $(SYMCRYPT_DYLIB)):$$LD_LIBRARYPATH \
	  SYMCRYPT_LIB_PATH=$(SYMCRYPT_HOME)/build/module/generic/ \
	  cargo test

# Approximation -- this recipe produces multiple targets
$(SYMCRYPT_HOME)/rs/src/sha3.rs: $(SYMCRYPT_SOURCES)
	[ x"$(SYMCRYPT_HOME)" != x ] # Error out because $$SYMCRYPT_HOME is empty
	./scylla $(SCYLLA_SYSROOT_OPT) --ccopts -DSYMCRYPT_IGNORE_PLATFORM,-I$(SYMCRYPT_HOME)/inc,-I$(SYMCRYPT_HOME)/build/inc,-std=gnu11,-DSCYLLA \
	  --errors_as_warnings --output $(dir $@) --bundle symcrypt_internal $(SYMCRYPT_SOURCES)

HACL_SOURCES= \
		Hacl_Chacha20.c \
		internal/Hacl_Bignum_Base.h Hacl_Bignum.c Hacl_Bignum4096.c \
		Hacl_Streaming_Types.h Hacl_Hash_SHA2.c \
		internal/Hacl_Bignum25519_51.h Hacl_Curve25519_51.c

# We extract all of the tests into the same hacl directory
.PHONY: regen-outputs
regen-outputs: test-hacl
	for f in rs/*.rs; do cp $$f out/hacl/src/; done

test-hacl:
	./scylla $(SCYLLA_OPTS) $(addprefix test/hacl/, $(HACL_SOURCES)) --output out/hacl/src/

.PHONY: test-%
test-%: test/%.c $(wildcard test/include/*) scylla
	./scylla $(SCYLLA_OPTS) $< --output out/hacl/src/ --ignore_lib_errors

.PHONY: nix-magic
nix-magic:
	nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes

.PHONY: format-check
format-check:
	@if ! dune build @fmt >/dev/null 2>&1; then \echo "\033[0;31m⚠️⚠️⚠️ SUGGESTED: $(MAKE) format-apply\033[0;m"; fi

.PHONY: format-apply
format-apply:
	dune fmt >/dev/null || true
