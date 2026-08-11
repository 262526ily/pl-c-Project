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

    .globl D
    .data
    .align 2
D:
    .word 40

    .globl E
    .data
    .align 2
E:
    .word 50

    .text
    .globl main
main:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    li t0, 10
    sw t0, -28(fp)
    li t0, 20
    sw t0, -24(fp)
    li t0, 30
    sw t0, -20(fp)
    li t0, 40
    sw t0, -16(fp)
    li t0, 50
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

