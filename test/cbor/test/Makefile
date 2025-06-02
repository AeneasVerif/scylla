all: CBORDetTest

EVERPARSE_LIB_PATH = $(realpath ..)
EVERPARSE_INCLUDE_PATH = $(realpath ..)

CFLAGS += -O2 -Werror -I $(EVERPARSE_INCLUDE_PATH) \
  -I $(EVERPARSE_INCLUDE_PATH)/krml/include \
  -I $(EVERPARSE_INCLUDE_PATH)/krml/krmllib/dist/generic

.PHONY: all

.PHONY: CBORDetTest

CBORDetTest: CBORDetTest.exe
	./CBORDetTest.exe

OBJ_FILES=CBORDetTest.o main.o

$(OBJ_FILES): %.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# CBORDetTest.c: gen/GenCBORDetTest.ml
# 	./gen/CBORDetTest.sh

#CBORDetTest.exe: CBORDetTest.o $(EVERPARSE_LIB_PATH)/evercbor.a
CBORDetTest.exe: $(OBJ_FILES) $(EVERPARSE_LIB_PATH)/CBORDet.o
	$(CC) -o CBORDetTest.exe $^

clean:
	rm -f *.o *.exe
