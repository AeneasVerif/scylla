#include <inttypes.h>

int f(uint16_t *x, uint16_t *y) {
  int z = x[0] - y[0];
  return z;
}

int _main() {
  uint16_t x = 0, y = 1;
  return f(&x, &y) != 0xffffffff;
}
