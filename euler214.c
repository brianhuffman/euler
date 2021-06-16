#include <stdlib.h>
#include <stdio.h>

#define NMAX 40000000
#define SIZE (NMAX / 2)
#define LEN 25

int main(int argc, char *argv[])
{
  int i, j, n, p, q, k, l;
  long long int pp;
  long long int sum = 0;
  char *chain_length;

  // allocate SIZE bytes for chain_length array
  chain_length = malloc(SIZE);
  if (!chain_length) exit(1);

  // initialize array to all 2s
  for (i = 0; i < SIZE; i++) chain_length[i] = 2;

  for (i = 1; i < SIZE; i++) {

    // if not prime, then try next one
    if (chain_length[i] > 2) continue;

    // p is prime
    p = 2*i + 1;

    // calculate q*2^k = p-1
    for (q = i, k = 1; !(q & 1); q /= 2, k++);

    // calculate l = chain_length(p)
    l = chain_length[q/2] + k;

    // add p to sum if the length matches
    if (l == LEN) sum += p;

    // loop over powers of p
    for (pp = p; pp < NMAX; pp *= p) {
      // loop over multiples of pp
      for (j = pp/2; j < SIZE; j += pp) {
	chain_length[j] += l-2;
      }
    }
  }

  // print sum
  printf ("Result: %lli\n", sum);
}
