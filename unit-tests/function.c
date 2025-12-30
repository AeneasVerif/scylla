void init() {}
void init2(void) {}

typedef enum { kNeon } CPUFeature;
static int armCPUInfo(CPUFeature feature) { return 0; }
typedef int (*VP8CPUInfo)(CPUFeature feature);
VP8CPUInfo VP8GetCPUInfo = armCPUInfo;

int _main() {
  init();
  init2();
  return 0;
}
