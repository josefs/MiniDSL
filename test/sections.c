void test() {
  int a, b;
  #pragma omp sections
  {
    #pragma omp section
    a = 1;
    #pragma omp section
    b = 2;
  }
}
