#include<stdlib.h>

#define N 1000000000

void test() {
  int i = 0;
  int *a = malloc(N*sizeof(int));
  #pragma omp parallel for
    for(i = 0; i < N; i++) {
      a[i] = (i*i) % 7;
    }
  free(a);
}

int main(void) {
  test();
}
