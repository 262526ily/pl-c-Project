    .text

    .text
    .globl mod_large
mod_large:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    andi t0, t0, 4095
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_large
.L_epilogue_mod_large:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_huge
mod_huge:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    andi t0, t0, 32767
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_huge
.L_epilogue_mod_huge:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mul_large
mul_large:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    sw a0, -12(fp)
    sw a1, -16(fp)
    lw t0, -12(fp)
    lw t1, -16(fp)
    mul t0, t0, t1
    sw t0, -20(fp)
    lw a0, -20(fp)
    j .L_epilogue_mul_large
.L_epilogue_mul_large:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

    .text
    .globl main
main:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    li t0, 5000
    sw t0, -24(fp)
    lw a0, -24(fp)
    call mod_large
    sw a0, -28(fp)
    lw t0, -28(fp)
    sw t0, -20(fp)
    lw a0, -24(fp)
    call mod_huge
    sw a0, -32(fp)
    lw t0, -32(fp)
    sw t0, -16(fp)
    li a0, 1000
    li a1, 2000
    call mul_large
    sw a0, -36(fp)
    lw t0, -36(fp)
    sw t0, -12(fp)
    lw t0, -20(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -44(fp)
    lw a0, -44(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

