    .text

    .text
    .globl main
main:
    addi sp, sp, -80
    sw ra, 76(sp)
    sw fp, 72(sp)
    addi fp, sp, 80
    j L1
L0:
    lw t0, -20(fp)
    li t1, 555
    add t0, t0, t1
    sw t0, -56(fp)
    lw t0, -56(fp)
    sw t0, -20(fp)
L1:
L2:
    lw t0, -12(fp)
    li t1, 3
    slt t0, t0, t1
    sw t0, -60(fp)
    lw t0, -60(fp)
    beqz t0, L4
L3:
    lw t0, -16(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -64(fp)
    lw t0, -64(fp)
    sw t0, -16(fp)
    lw t0, -12(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -72(fp)
    lw t0, -72(fp)
    sw t0, -12(fp)
    j L2
L4:
    lw t0, -20(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -76(fp)
    lw t0, -76(fp)
    sw t0, -20(fp)
    lw a0, -20(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 80
    ret

