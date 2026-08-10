Semantic check success!
global A = 10
global B = 20
global C = 30

func main():
  locals: [v$4, w$3, z$2, y$1, x$0]
  temps: 6

entry:
  t0 = A + B
  x$0 = t0
  t1 = C - A
  y$1 = t1
  t2 = x$0 * y$1
  z$2 = t2
  t3 = z$2 / 3
  w$3 = t3
  t4 = w$3 % 7
  v$4 = t4
  t5 = v$4 + 2
  return t5
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
    addi sp, sp, -64
    sw ra, 60(sp)
    sw fp, 56(sp)
    addi fp, sp, 64
    la t0, A
    lw t0, 0(t0)
    la t1, B
    lw t1, 0(t1)
    add t0, t0, t1
    sw t0, -32(fp)
    lw t0, -32(fp)
    sw t0, -28(fp)
    la t0, C
    lw t0, 0(t0)
    la t1, A
    lw t1, 0(t1)
    sub t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -24(fp)
    lw t0, -28(fp)
    lw t1, -24(fp)
    mul t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -20(fp)
    lw t0, -20(fp)
    li t1, 3
    div t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -16(fp)
    lw t0, -16(fp)
    li t1, 7
    rem t0, t0, t1
    sw t0, -48(fp)
    lw t0, -48(fp)
    sw t0, -12(fp)
    lw t0, -12(fp)
    li t1, 2
    add t0, t0, t1
    sw t0, -52(fp)
    lw a0, -52(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 64
    ret

