Semantic check success!
global N = 10

func main():
  locals: [i$1, sum$0]
  temps: 3

entry:
  sum$0 = 0
  i$1 = 0
L0:
  t0 = i$1 < N
  ifFalse t0 goto L2
L1:
  t1 = sum$0 + i$1
  sum$0 = t1
  t2 = i$1 + 1
  i$1 = t2
  goto L0
L2:
  return sum$0
    .text

    .globl N
    .data
    .align 2
N:
    .word 10

    .text
    .globl main
main:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    li t0, 0
    sw t0, -16(fp)
    li t0, 0
    sw t0, -12(fp)
L0:
    lw t0, -12(fp)
    la t1, N
    lw t1, 0(t1)
    slt t0, t0, t1
    sw t0, -20(fp)
    lw t0, -20(fp)
    beqz t0, L2
L1:
    lw t0, -16(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -24(fp)
    lw t0, -24(fp)
    sw t0, -16(fp)
    lw t0, -12(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -28(fp)
    lw t0, -28(fp)
    sw t0, -12(fp)
    j L0
L2:
    lw a0, -16(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

