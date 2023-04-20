# include <stdio.h>

int main()
{
  asm volatile("addi a0, x0, 1");
  asm volatile("addi a1, x0, 2");
  asm volatile("jal ra, jump"); // ジャンプ

  asm volatile("addi a0, x0, 2"); // 実行されない命令
  asm volatile("addi a1, x0, 3"); // 実行されない命令

  // ジャンプ先
  asm volatile("jump:");
  asm volatile("nop");
  asm volatile("nop");
  asm volatile("nop");
  asm volatile("nop");
  asm volatile("add a2, a0, a1");
  asm volatile("nop");
  asm volatile("nop");
  asm volatile("nop");
  asm volatile("nop");

  asm volatile("unimp");

  return 0;
}
