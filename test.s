    .text

    .text
    .globl main
main:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    li t0, 14
    sw t0, -20(fp)
    li t0, 5
    sw t0, -16(fp)
    lw t0, -20(fp)
    lw t1, -16(fp)
    mul t0, t0, t1
    sw t0, -24(fp)
    lw t0, -24(fp)
    li t1, 5
    add t0, t0, t1
    sw t0, -28(fp)
    lw t0, -28(fp)
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

