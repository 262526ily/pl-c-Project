    .text

    .text
    .globl mod_2048
mod_2048:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    andi t0, t0, 2047
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_2048
.L_epilogue_mod_2048:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_4096
mod_4096:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 4095
    and t0, t0, t1
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_4096
.L_epilogue_mod_4096:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_8192
mod_8192:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 8191
    and t0, t0, t1
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_8192
.L_epilogue_mod_8192:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_16384
mod_16384:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 16383
    and t0, t0, t1
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_16384
.L_epilogue_mod_16384:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_32768
mod_32768:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 32767
    and t0, t0, t1
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_32768
.L_epilogue_mod_32768:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_65536
mod_65536:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 65535
    and t0, t0, t1
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_mod_65536
.L_epilogue_mod_65536:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl mod_runtime
mod_runtime:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    sw a0, -12(fp)
    sw a1, -16(fp)
    lw t0, -12(fp)
    lw t1, -16(fp)
    rem t0, t0, t1
    sw t0, -20(fp)
    lw a0, -20(fp)
    j .L_epilogue_mod_runtime
.L_epilogue_mod_runtime:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

    .text
    .globl main
main:
    addi sp, sp, -96
    sw ra, 92(sp)
    sw fp, 88(sp)
    addi fp, sp, 96
    li t0, 50000
    sw t0, -40(fp)
    lw a0, -40(fp)
    call mod_2048
    sw a0, -44(fp)
    lw t0, -44(fp)
    sw t0, -36(fp)
    lw a0, -40(fp)
    call mod_4096
    sw a0, -48(fp)
    lw t0, -48(fp)
    sw t0, -32(fp)
    lw a0, -40(fp)
    call mod_8192
    sw a0, -52(fp)
    lw t0, -52(fp)
    sw t0, -28(fp)
    lw a0, -40(fp)
    call mod_16384
    sw a0, -56(fp)
    lw t0, -56(fp)
    sw t0, -24(fp)
    lw a0, -40(fp)
    call mod_32768
    sw a0, -60(fp)
    lw t0, -60(fp)
    sw t0, -20(fp)
    lw a0, -40(fp)
    call mod_65536
    sw a0, -64(fp)
    lw t0, -64(fp)
    sw t0, -16(fp)
    lw a0, -40(fp)
    li a1, 7
    call mod_runtime
    sw a0, -68(fp)
    lw t0, -68(fp)
    sw t0, -12(fp)
    lw t0, -36(fp)
    lw t1, -32(fp)
    add t0, t0, t1
    sw t0, -72(fp)
    lw t0, -72(fp)
    lw t1, -28(fp)
    add t0, t0, t1
    sw t0, -76(fp)
    lw t0, -76(fp)
    lw t1, -24(fp)
    add t0, t0, t1
    sw t0, -80(fp)
    lw t0, -80(fp)
    lw t1, -20(fp)
    add t0, t0, t1
    sw t0, -84(fp)
    lw t0, -84(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -88(fp)
    lw t0, -88(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -92(fp)
    lw a0, -92(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 96
    ret

