#include <stdio.h>
#include <stddef.h>

int main() {
  printf("(* This file is auto-generated from misc/data_model.c *)\n");
  printf("let size_long = %zu\n", sizeof(long));
  printf("let size_size = %zu\n", sizeof(size_t));
  return 0;
}
