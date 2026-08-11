Semantic check success!
global A = 10
global B = 20
global C = 30

func main():
  locals: [sum$3, z$2, y$1, x$0]
  temps: 2

entry:
  x$0 = A
  y$1 = B
  z$2 = C
  t0 = x$0 + y$1
  t1 = t0 + z$2
  sum$3 = t1
  return sum$3
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
    .word 20

    .globl C
    .data
    .align 2
C:
    .word 30

    .text
    .globl main
main:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    li t0, 10
    li t0, 20
    li t0, 30
    add t0, t0, t1
    mv t0, t2
    mv t1, t3
    add t0, t0, t1
    mv t4, t5
    mv a0, t4
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

