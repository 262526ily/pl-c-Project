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

    .text
    .globl main
main:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    li t0, 30
    sw t0, -28(fp)
    li t0, 30
    sw t0, -24(fp)
    li t0, 60
    sw t0, -32(fp)
    li t0, 60
    sw t0, -20(fp)
    li t0, 30
    sw t0, -36(fp)
    li t0, 30
    sw t0, -16(fp)
    li t0, 10
    sw t0, -40(fp)
    li t0, 10
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

