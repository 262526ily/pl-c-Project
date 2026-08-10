Semantic check success!
global N = 10
global THRESHOLD = 5

func main():
  locals: [step$2, i$1, sum$0]
  temps: 7

entry:
  sum$0 = 0
  i$1 = 0
  t0 = THRESHOLD * 10
  step$2 = t0
L0:
  t1 = i$1 < N
  ifFalse t1 goto L2
L1:
  t2 = i$1 < THRESHOLD
  ifFalse t2 goto L3
  t3 = sum$0 + step$2
  sum$0 = t3
  goto L4
L3:
  t4 = step$2 * 2
  t5 = sum$0 + t4
  sum$0 = t5
L4:
  t6 = i$1 + 1
  i$1 = t6
  goto L0
L2:
  return sum$0
    .text

    .globl N
    .data
    .align 2
N:
    .word 10

    .globl THRESHOLD
    .data
    .align 2
THRESHOLD:
    .word 5

    .text
    .globl main
main:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    li t0, 0
    sw t0, -20(fp)
    li t0, 0
    sw t0, -16(fp)
    la t0, THRESHOLD
    lw t0, 0(t0)
    slli t1, t0, 3
    slli t2, t0, 1
    add t0, t1, t2
    sw t0, -24(fp)
    lw t0, -24(fp)
    sw t0, -12(fp)
L0:
    lw t0, -16(fp)
    la t1, N
    lw t1, 0(t1)
    slt t0, t0, t1
    sw t0, -28(fp)
    lw t0, -28(fp)
    beqz t0, L2
L1:
    lw t0, -16(fp)
    la t1, THRESHOLD
    lw t1, 0(t1)
    slt t0, t0, t1
    sw t0, -32(fp)
    lw t0, -32(fp)
    beqz t0, L3
    lw t0, -20(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -20(fp)
    j L4
L3:
    lw t0, -12(fp)
    slli t0, t0, 1
    sw t0, -40(fp)
    lw t0, -20(fp)
    lw t1, -40(fp)
    add t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -20(fp)
L4:
    lw t0, -16(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -48(fp)
    lw t0, -48(fp)
    sw t0, -16(fp)
    j L0
L2:
    lw a0, -20(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

