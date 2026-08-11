    .text

    .text
    .globl fib
fib:
    addi sp, sp, -64
    sw ra, 60(sp)
    sw fp, 56(sp)
    addi fp, sp, 64
    sw a0, -12(fp)
    li t0, 0
    sw t0, -32(fp)
    lw t0, -12(fp)
    li t1, 1
    slt t0, t1, t0
    xori t0, t0, 1
    sw t0, -36(fp)
    lw t0, -36(fp)
    beqz t0, L0
    lw t0, -12(fp)
    sw t0, -32(fp)
    j L1
L0:
    lw t0, -12(fp)
    li t1, 1
    sub t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -28(fp)
    lw t0, -12(fp)
    li t1, 2
    sub t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -24(fp)
    lw a0, -28(fp)
    call fib
    sw a0, -48(fp)
    lw t0, -48(fp)
    sw t0, -20(fp)
    lw a0, -24(fp)
    call fib
    sw a0, -52(fp)
    lw t0, -52(fp)
    sw t0, -16(fp)
    lw t0, -20(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -56(fp)
    lw t0, -56(fp)
    sw t0, -32(fp)
L1:
    lw a0, -32(fp)
    j .L_epilogue_fib
.L_epilogue_fib:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 64
    ret

    .text
    .globl main
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    li a0, 8
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

