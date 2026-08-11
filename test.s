Semantic check success!
global A = 10
global B = 5

func main():
  locals: [y$1, x$0]
  temps: 1

entry:
  x$0 = A
  y$1 = B
  t0 = x$0 < y$1
  ifFalse t0 goto L0
  return x$0
  goto L1
L0:
  return y$1
L1:
    .text

    .globl A
    .data
    .align 2
A:
    .word 10

    .globl B
    .data
    .align 2
B:
    .word 5

    .text
    .globl main
main:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
entry:
    li t0, 10
    sw t0, -16(fp)
    li t0, 5
    sw t0, -12(fp)
    lw t0, -16(fp)
    lw t1, -12(fp)
    slt t0, t0, t1
    sw t0, -20(fp)
    lw t0, -20(fp)
    beqz t0, L0
    lw a0, -16(fp)
    j .L_epilogue_main
L0:
    lw a0, -12(fp)
    j .L_epilogue_main
L1:
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

