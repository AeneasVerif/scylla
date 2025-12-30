int g() { return 0; }
int (*f)() = g;

int main() { return f(); }
