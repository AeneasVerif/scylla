# We need the sysroot for the version of llvm clangml was built against. Running brew takes ages so
# we cache the result. No assumptions made on the underlying shell or gnu-ness of sed.
ifeq ($(shell uname -s),Darwin)
  ISYSROOT=$(shell [ -f .sysroot ] && cat .sysroot || \
    brew info llvm@15 | egrep '^/' | sed 's!\(15\.[^ ]*\) .*!\1/Toolchains/LLVM\1.xctoolchain!' | tee .sysroot)
  ifneq ($(ISYSROOT),)
    SCYLLA_SYSROOT_OPT = --ccopts -isysroot,$(ISYSROOT)/
  endif
endif

ifeq ($(shell uname -n),arm64)
  PQCRYPTO_ARCH_ARG=ARCH=ARM
endif

SYMCRYPT_HOME 	?= ../SymCrypt
BZIP2_HOME 	?= ../bzip2
PQCRYPTO_HOME 	?= ../PQCrypto-LWEKE

# We try to figure out the best include paths, compiler options, etc. from the build system.
SCYLLA_OPTS += --errors_as_warnings $(SCYLLA_SYSROOT_OPT) --ignore_lib_errors
HACL_OPTS = --auto-box --ccopts -DKRML_UNROLL_MAX=0,-I,test/hacl/include,-I,test/hacl/
CBOR_OPTS = --ccopts -I,test/cbor/,-I,test/cbor/krml/include,-I,test/cbor/krml/krmllib/dist/generic

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
test: regen-outputs test-cbor test-symcrypt test-pqcrypto test-bzip2 unit-tests
	cd out/hacl && cargo test
	cd out/cbor && cargo test

# SYMCRYPT
# --------

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

# FRODO
# -----

.PHONY: test-pqcrypto
test-pqcrypto:
	$(MAKE) -C $(PQCRYPTO_HOME)/FrodoKEM $(PQCRYPTO_ARCH_ARG) OPT_LEVEL=FAST_GENERIC USE_OPENSSL=FALSE tests-rs

# BZIP2
# -----

BZLIB_OBJS = $(addprefix $(BZIP2_HOME)/build/CMakeFiles/,bzip2.dir/bzip2.c.o bz2_ObjLib.dir/decompress.c.o bz2_ObjLib.dir/bzlib.c.o)

.PHONY: test-bzip2
test-bzip2: $(BZIP2_HOME)/bzip2-rs/target/release/libbzip2_rs.a build-bzip2
	$(CC) $(BZLIB_OBJS) $< -o $(BZIP2_HOME)/build/bzip2
	cd $(BZIP2_HOME)/build && RUST_BACKTRACE=1 ctest -V

.PHONY: build-bzip2
build-bzip2:
	cd $(BZIP2_HOME) && mkdir -p build && cd build && cmake .. && cmake --build .

# One rule in two steps because otherwise it's a mess GNU Make can't expression one recipe -
# multiple productions
$(BZIP2_HOME)/bzip2-rs/target/release/libbzip2_rs.a: $(wildcard $(BZIP2_HOME)/*.c $(BZIP2_HOME)/*.h)
	./scylla --ccopts -I$(BZIP2_HOME),-DSCYLLA --errors_as_warnings --ignore_lib_errors --bundle bzlib_private $(addprefix $(BZIP2_HOME)/,scylla_glue.h bzlib_private.h blocksort.c huffman.c crctable.c randtable.c compress.c) --output $(BZIP2_HOME)/bzip2-rs/src/
	cd $(BZIP2_HOME)/bzip2-rs && cargo build --release

# CBOR

.PHONY: test-cbor
test-cbor: test/cbor/CBORDet.c test/cbor/CBORDet.h scylla
	./scylla $(CBOR_OPTS) $(SCYLLA_OPTS) test/cbor/CBORDet.c --output out/cbor/src

# HACL
# ----

HACL_SOURCES= \
		Hacl_Chacha20.c \
		internal/Hacl_Bignum_Base.h Hacl_Bignum.c Hacl_Bignum4096.c \
		Hacl_Streaming_Types.h Hacl_Hash_SHA2.c \
		internal/Hacl_Bignum25519_51.h Hacl_Curve25519_51.c \
		Hacl_MAC_Poly1305.c \
		internal/Hacl_P256_PrecompTable.h Hacl_P256.c \
		Hacl_AEAD_Chacha20Poly1305.c

# We extract all of the tests into the same hacl directory
.PHONY: regen-outputs
regen-outputs: test-hacl
	for f in rs/*.rs; do cp $$f out/hacl/src/; done

.PHONY: test-hacl
test-hacl: $(addprefix test/hacl/, $(HACL_SOURCES)) scylla
	./scylla $(HACL_OPTS) $(SCYLLA_OPTS) $(addprefix test/hacl/, $(HACL_SOURCES)) --output out/hacl/src/

# UNIT TESTS
# ----------

ALL_UNIT_TESTS = $(patsubst %.c,%.test,$(wildcard unit-tests/*.c))
BROKEN_UNIT_TESTS = unit-tests/function_pointers.test
UNIT_TESTS = $(filter-out $(BROKEN_UNIT_TESTS),$(ALL_UNIT_TESTS))

unit-tests: $(UNIT_TESTS)

unit-tests/%.test: unit-tests/%.exe
	$<

unit-tests/%.exe: unit-tests/%.rs
	rustc $< -o $@

.PRECIOUS: unit-tests/%.rs
unit-tests/%.rs: unit-tests/%.c build
	./scylla --output $(dir $@) $<
	echo "fn main() { assert_eq!(0, _main()) }" >> $@

# MISC
# ----

.PHONY: nix-magic
nix-magic:
	nix flake update --extra-experimental-features nix-command --extra-experimental-features flakes

.PHONY: format-check
format-check:
	@if ! dune build @fmt >/dev/null 2>&1; then \echo "\033[0;31m⚠️⚠️⚠️ SUGGESTED: $(MAKE) format-apply\033[0;m"; fi

.PHONY: format-apply
format-apply:
	dune fmt >/dev/null || true

