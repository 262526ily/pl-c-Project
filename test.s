    .text

    .globl N
    .data
    .align 2
N:
    .word 20

    .globl A
    .data
    .align 2
A:
    .word 5

    .text
    .globl main
main:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    lw t0, -48(fp)
    sw t0, -16(fp)
    li t0, 0
    sw t0, -20(fp)
    li t0, 0
    sw t0, -16(fp)
    li t0, 10
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
    li t1, 10
    slt t0, t1, t0
    sw t0, -32(fp)
    lw t0, -32(fp)
    beqz t0, L3
    lw t0, -12(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -20(fp)
    lw t1, -36(fp)
    add t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -20(fp)
    j L4
L3:
    lw t0, -20(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -20(fp)
L4:
    lw t0, -16(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -48(fp)
    j L0
L2:
    lw a0, -20(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

