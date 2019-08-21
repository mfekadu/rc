#include <stdio.h>
#include <inttypes.h>

int64_t read_int() {
  int64_t i;
  scanf("%" SCNd64, &i);
  return i;
}

int main() { read_int(); }
