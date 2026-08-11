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
    .word 5

    .text
    .globl main
main:
    addi sp, sp, -160
    sw ra, 156(sp)
    sw fp, 152(sp)
    addi fp, sp, 160
    li t0, 30
    sw t0, -60(fp)
    lw t0, -60(fp)
    sw t0, -56(fp)
    li t0, 20
    sw t0, -64(fp)
    lw t0, -64(fp)
    sw t0, -52(fp)
    li t0, 200
    sw t0, -68(fp)
    lw t0, -68(fp)
    sw t0, -48(fp)
    li t0, 6
    sw t0, -72(fp)
    lw t0, -72(fp)
    sw t0, -44(fp)
    li t0, 0
    sw t0, -76(fp)
    lw t0, -76(fp)
    sw t0, -40(fp)
    li t0, 30
    sw t0, -80(fp)
    lw t0, -80(fp)
    lw t1, -80(fp)
    add t0, t0, t1
    sw t0, -84(fp)
    lw t0, -84(fp)
    sw t0, -36(fp)
    li t0, 20
    sw t0, -88(fp)
    lw t0, -88(fp)
    la t1, D
    lw t1, 0(t1)
    div t0, t0, t1
    sw t0, -92(fp)
    lw t0, -92(fp)
    sw t0, -32(fp)
    li t0, 200
    sw t0, -96(fp)
    lw t0, -96(fp)
    la t1, C
    lw t1, 0(t1)
    add t0, t0, t1
    sw t0, -100(fp)
    lw t0, -100(fp)
    sw t0, -28(fp)
    li t0, 0
    sw t0, -104(fp)
    lw t0, -104(fp)
    sw t0, -24(fp)
    li t0, 1
    sw t0, -108(fp)
    lw t0, -108(fp)
    sw t0, -20(fp)
    li t0, 1
    sw t0, -116(fp)
    lw t0, -116(fp)
    sw t0, -112(fp)
    lw t0, -112(fp)
    beqz t0, L0
    li t0, 1
    sw t0, -120(fp)
    lw t0, -120(fp)
    sw t0, -112(fp)
    j L1
L0:
    li t0, 0
    sw t0, -112(fp)
L1:
    lw t0, -112(fp)
    sw t0, -16(fp)
    lw t0, -56(fp)
    lw t1, -52(fp)
    add t0, t0, t1
    sw t0, -124(fp)
    lw t0, -124(fp)
    lw t1, -48(fp)
    add t0, t0, t1
    sw t0, -128(fp)
    lw t0, -128(fp)
    lw t1, -44(fp)
    add t0, t0, t1
    sw t0, -132(fp)
    lw t0, -132(fp)
    lw t1, -40(fp)
    add t0, t0, t1
    sw t0, -136(fp)
    lw t0, -136(fp)
    lw t1, -36(fp)
    add t0, t0, t1
    sw t0, -140(fp)
    lw t0, -140(fp)
    lw t1, -32(fp)
    add t0, t0, t1
    sw t0, -144(fp)
    lw t0, -144(fp)
    lw t1, -28(fp)
    add t0, t0, t1
    sw t0, -148(fp)
    lw t0, -148(fp)
    lw t1, -24(fp)
    add t0, t0, t1
    sw t0, -152(fp)
    lw t0, -152(fp)
    lw t1, -20(fp)
    add t0, t0, t1
    sw t0, -156(fp)
    lw t0, -156(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -160(fp)
    lw t0, -160(fp)
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 160
    ret

