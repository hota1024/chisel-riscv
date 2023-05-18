# include <stdio.h>

int main()
{
  unsigned int size = 5;
  unsigned int vl;

  while (size > 0)
  {
    asm volatile(
      "vsetvli %0, %1, e32, m1"
      : "=r"(vl)
      : "r"(size)
    );

    size -= vl;
  }

  asm volatile("unimp");
  return 0;
}
