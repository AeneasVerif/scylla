typedef struct {
  int x, y;
} point;

typedef struct {
  point base;
  int magnitude[2];
} vector;

int _main () {
  vector v = { .base = { .x = 0, .y = 0 }, .magnitude = { 0, 0 } };
  // TODO: would not work with vs[]
  vector vs[1] = {{ .base = { .x = 0, .y = 0 }, .magnitude = { 0, 0 } }};
  return v.base.x != vs[0].magnitude[0];

}
