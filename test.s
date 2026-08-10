Semantic check success!
global A = 3

func main():
  locals: [x$0]
  temps: 1

entry:
  x$0 = 999
  t0 = A > 5
  ifFalse t0 goto L0
  x$0 = 100
  goto L1
L0:
  x$0 = 200
L1:
  return x$0
    .text

    .globl A
    .data
    .align 2
A:
    .word 3

    .text
    .globl main
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li t0, 999
    sw t0, -12(fp)
    li t0, 0
    sw t0, -16(fp)
    lw t0, -16(fp)
    beqz t0, L0
    li t0, 100
    sw t0, -12(fp)
    j L1
L0:
    li t0, 200
    sw t0, -12(fp)
L1:
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

