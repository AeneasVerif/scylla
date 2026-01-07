int _main() {
  int i = 0;
  do {
    i++;
  } while (++i < 0);
  return (++i) == 0;
}
