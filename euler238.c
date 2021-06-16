#include <stdlib.h>
#include <stdio.h>

/*
s_(0)   = 14025256
s_(n+1) = s_(n)^(2) mod 20300713

20300713 = 4127 * 4919

multiplicative order of 1402526 (mod 20300713) = 5072917
multiplicative order of 1402526 (mod 4127) = 2063
multiplicative order of 1402526 (mod 4919) = 2459

s (mod 4127) loops with period 1031.
s (mod 4919) loops with period 2458.
s loops with period 2534198 = lcm 1031 2458.

Number of digits in repeating portion = 18886117.
Sum    of digits in repeating portion = 80846691.
*/

#define COUNT 2534198
#define SIZE 18886117
#define INIT 14025256
#define MODULUS 20300713

int main(int argc, char *argv[])
{
  int i, j, k, n, sum, remaining;
  long long int a;
  long long int total;
  char *s, *digits;
  int *sums, *p;

  // allocate SIZE bytes for digits char array
  digits = malloc(SIZE);
  if (!digits) exit(1);

  // allocate 4*SIZE bytes for sums array
  sums = malloc(sizeof(int) * SIZE);
  if (!sums) exit(1);

  // initialize digits array
  for (i = 0, s = digits, a = INIT; i < COUNT; i++) {
    n = sprintf(s, "%lli", a);
    s += n;
    a = (a * a) % MODULUS;
  }

  // initialize sums array
  for (i = 0, sum = 0; i < SIZE; i++) {
    sums[i] = sum;
    sum += digits[i] - '0';
  }

  // allocate 4*sum bytes for p array
  p = malloc(sizeof(int) * sum);
  if (!p) exit(1);

  // clear p array
  for (i = 0; i < sum; i++) p[i] = 0;

  // count remaining k such that p(k) = 0
  remaining = sum;

  // mark all k such that p(k) = i+1
  for (i = 0; remaining > 0; i++) {
    for (j = 0; j < SIZE; j++) {
      k = sums[j] - sums[i];
      if (k < 0) k += sum;
      if (p[k] == 0) {
        p[k] = i+1;
        remaining--;
      }
    }
  }

  // print maximum value of p(k)
  printf ("Max p(k): %i\n", i+1);

  // print p(k) for small k
  for (k = 0; k < 20; k++) {
    printf ("p(%i) = %i\n", k, p[k]);
  }

  // print sum
  printf ("Sum: %i\n", sum);

  // print sum of p(k) up to 10^15
  for (k = 0, total = 0; k < sum; k++) {
    total += p[k];
  }
  total *= 24738180;
  // total *= 1;
  // for (k = 0; k <= 2818810; k++) {
  // for (k = 0; k <= 1000; k++) {
  for (k = 0; k <= 5637620; k++) {
    total += p[k];
  }
  total -= 1; // fix value for p[0]
  printf ("Total: %lli\n", total);

  // 4961272552276189 WRONG!  (should be right)
  // 4961272552276190 WRONG!

}
