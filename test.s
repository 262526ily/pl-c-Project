Semantic check success!

func main():
  locals: [c$4, b$3, a$2, i$1, sum$0]
  temps: 8

entry:
  sum$0 = 0
  i$1 = 0
L0:
  t0 = i$1 < 100
  ifFalse t0 goto L2
L1:
  t1 = i$1 * 2
  a$2 = t1
  t2 = i$1 * 2
  b$3 = t2
  t3 = i$1 * 2
  c$4 = t3
  t4 = sum$0 + a$2
  t5 = t4 + b$3
  t6 = t5 + c$4
  sum$0 = t6
  t7 = i$1 + 1
  i$1 = t7
  goto L0
L2:
  return sum$0
    .text

    .text
    .globl main
main:
    addi sp, sp, -64
    sw ra, 60(sp)
    sw fp, 56(sp)
    addi fp, sp, 64
    li t0, 0
    sw t0, -28(fp)
    li t0, 0
    sw t0, -24(fp)
L0:
    lw t0, -24(fp)
    li t1, 100
    slt t0, t0, t1
    sw t0, -32(fp)
    lw t0, -32(fp)
    beqz t0, L2
L1:
    lw t0, -24(fp)
    lw t1, -24(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -20(fp)
    lw t0, -36(fp)
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -16(fp)
    lw t0, -36(fp)
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -12(fp)
    lw t0, -28(fp)
    lw t1, -20(fp)
    add t0, t0, t1
    sw t0, -48(fp)
    lw t0, -48(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -52(fp)
    lw t0, -52(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -56(fp)
    lw t0, -56(fp)
    sw t0, -28(fp)
    lw t0, -24(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -60(fp)
    lw t0, -60(fp)
    sw t0, -24(fp)
    j L0
L2:
    lw a0, -28(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 64
    ret

