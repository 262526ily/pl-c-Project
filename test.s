Semantic check success!
global A = 10
global B = 20
global C = 30
global N = 50

func main():
  locals: [limit$3, step$2, i$1, sum$0]
  temps: 5

entry:
  sum$0 = 0
  i$1 = 0
  t0 = A + B
  step$2 = t0
  t1 = N + C
  limit$3 = t1
L0:
  t2 = i$1 < limit$3
  ifFalse t2 goto L2
L1:
  t3 = sum$0 + step$2
  sum$0 = t3
  t4 = i$1 + 1
  i$1 = t4
  goto L0
L2:
  return sum$0
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

    .globl N
    .data
    .align 2
N:
    .word 50

    .text
    .globl main
main:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    lw t0, -44(fp)
    sw t0, -20(fp)
    lw t0, -40(fp)
    sw t0, -24(fp)
    li t0, 0
    sw t0, -24(fp)
    li t0, 0
    sw t0, -20(fp)
    li t0, 30
    sw t0, -28(fp)
    lw t0, -28(fp)
    sw t0, -16(fp)
    li t0, 80
    sw t0, -32(fp)
    lw t0, -32(fp)
    sw t0, -12(fp)
L0:
    lw t0, -20(fp)
    lw t1, -12(fp)
    slt t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    beqz t0, L2
L1:
    lw t0, -24(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -40(fp)
    lw t0, -20(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -44(fp)
    j L0
L2:
    lw a0, -24(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

