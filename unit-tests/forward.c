typedef struct point point;

struct point {
  int x, y;
};

int _main() {
  point p = { 0, 0 };
  return p.x + p.y;
}
