    .text

    .text
    .globl sum
sum:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    sw a0, -12(fp)
    sw a1, -16(fp)
    lw t0, -12(fp)
    li t1, 0
    slt t0, t1, t0
    xori t0, t0, 1
    sw t0, -20(fp)
    lw t0, -20(fp)
    beqz t0, L0
    lw a0, -16(fp)
    j .L_epilogue_sum
L0:
    lw t0, -12(fp)
    li t1, 1
    sub t0, t0, t1
    sw t0, -24(fp)
    lw t0, -16(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -28(fp)
    lw a0, -24(fp)
    lw a1, -28(fp)
    call sum
    sw a0, -32(fp)
    lw a0, -32(fp)
    j .L_epilogue_sum
L1:
.L_epilogue_sum:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

    .text
    .globl main
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li a0, 100
    li a1, 0
    call sum
    sw a0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

