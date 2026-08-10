Semantic check success!

func mul(a$0, b$1):
  locals: [b$1, a$0]
  temps: 1

entry:
  t0 = a$0 * b$1
  return t0

func main():
  locals: [z$2, y$1, x$0]
  temps: 1

entry:
  x$0 = 10
  y$1 = 20
  param y$1
  param x$0
  t0 = call mul, 2
  z$2 = t0
  return z$2
    .text

    .text
    .globl mul
mul:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    sw a0, -12(fp)
    sw a1, -16(fp)
    lw t0, -12(fp)
    lw t1, -16(fp)
    mul t0, t0, t1
    sw t0, -20(fp)
    lw a0, -20(fp)
    j .L_epilogue_mul
.L_epilogue_mul:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

    .text
    .globl main
main:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    li t0, 10
    sw t0, -20(fp)
    li t0, 20
    sw t0, -16(fp)
    lw a0, -20(fp)
    lw a1, -16(fp)
    call mul
    sw a0, -24(fp)
    lw t0, -24(fp)
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

