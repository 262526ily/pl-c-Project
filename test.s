    .text

    .text
    .globl fib
fib:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    sw a0, -12(fp)
    li t0, 0
    sw t0, -28(fp)
    li t0, 1
    sw t0, -24(fp)
    li t0, 0
    sw t0, -20(fp)
    li t0, 0
    sw t0, -16(fp)
L0:
    lw t0, -20(fp)
    lw t1, -12(fp)
    slt t0, t0, t1
    sw t0, -32(fp)
    lw t0, -32(fp)
    beqz t0, L2
L1:
    lw t0, -28(fp)
    lw t1, -24(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -16(fp)
    lw t0, -24(fp)
    sw t0, -28(fp)
    lw t0, -16(fp)
    sw t0, -24(fp)
    lw t0, -20(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -20(fp)
    j L0
L2:
    lw a0, -28(fp)
    j .L_epilogue_fib
.L_epilogue_fib:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

    .text
    .globl main
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li a0, 5
    call fib
    sw a0, -16(fp)
    lw t0, -16(fp)
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

