all: extract

OUTPUT_DIRECTORY:=.
KRML_HOME:=./krml

extract: $(OUTPUT_DIRECTORY)/CBORDet.o

test: extract
	+$(MAKE) -C $@

.PHONY: all extract test

CFLAGS += -O2 -I $(KRML_HOME)/include -I $(KRML_HOME)/krmllib/dist/generic

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f *.o
	make -C test clean
