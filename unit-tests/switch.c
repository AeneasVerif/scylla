typedef enum { A, B, C } e;

int _main() {
  int r = 0;
  switch (A) {
    case B:
    case C:
      r = 1;
      break;
    case A:
      r = 0;
      break;
  }
  return r;
}
