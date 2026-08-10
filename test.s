    .text

    .text
    .globl test_unused_var
test_unused_var:
    addi sp, sp, -32
    sw ra, 28(sp)
    sw fp, 24(sp)
    addi fp, sp, 32
    li t0, 10
    sw t0, -20(fp)
    li t0, 20
    sw t0, -16(fp)
    lw t0, -20(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -24(fp)
    lw t0, -24(fp)
    sw t0, -12(fp)
    li a0, 100
    j .L_epilogue_test_unused_var
.L_epilogue_test_unused_var:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 32
    ret

    .text
    .globl test_partial_use
test_partial_use:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    sw a0, -12(fp)
    lw t0, -12(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -28(fp)
    lw t0, -28(fp)
    sw t0, -24(fp)
    lw t0, -12(fp)
    slli t1, t0, 1
    add t0, t0, t1
    sw t0, -32(fp)
    lw t0, -32(fp)
    sw t0, -20(fp)
    lw t0, -24(fp)
    li t1, 5
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_test_partial_use
.L_epilogue_test_partial_use:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

    .text
    .globl test_loop_dead
test_loop_dead:
    addi sp, sp, -48
    sw ra, 44(sp)
    sw fp, 40(sp)
    addi fp, sp, 48
    li t0, 0
    sw t0, -24(fp)
    li t0, 0
    sw t0, -20(fp)
    li t0, 100
    sw t0, -16(fp)
L0:
    lw t0, -20(fp)
    li t1, 10
    slt t0, t0, t1
    sw t0, -28(fp)
    lw t0, -28(fp)
    beqz t0, L2
L1:
    lw t0, -20(fp)
    lw t1, -20(fp)
    add t0, t0, t1
    sw t0, -32(fp)
    lw t0, -32(fp)
    sw t0, -12(fp)
    lw t0, -24(fp)
    lw t1, -20(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -24(fp)
    lw t0, -20(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -20(fp)
    j L0
L2:
    lw a0, -24(fp)
    j .L_epilogue_test_loop_dead
.L_epilogue_test_loop_dead:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 48
    ret

    .text
    .globl test_if_dead
test_if_dead:
    addi sp, sp, -64
    sw ra, 60(sp)
    sw fp, 56(sp)
    addi fp, sp, 64
    sw a0, -12(fp)
    li t0, 0
    sw t0, -32(fp)
    lw t0, -12(fp)
    lw t1, -12(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -28(fp)
    lw t0, -12(fp)
    li t1, 5
    slt t0, t1, t0
    sw t0, -40(fp)
    lw t0, -40(fp)
    beqz t0, L3
    lw t0, -28(fp)
    slli t1, t0, 1
    add t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -24(fp)
    li t0, 100
    sw t0, -32(fp)
    j L4
L3:
    lw t0, -28(fp)
    li t1, 10
    add t0, t0, t1
    sw t0, -48(fp)
    lw t0, -48(fp)
    sw t0, -20(fp)
    li t0, 0
    sw t0, -32(fp)
L4:
    lw t0, -32(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -52(fp)
    lw t0, -52(fp)
    sw t0, -16(fp)
    lw a0, -16(fp)
    j .L_epilogue_test_if_dead
.L_epilogue_test_if_dead:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 64
    ret

    .text
    .globl test_multi_dead
test_multi_dead:
    addi sp, sp, -64
    sw ra, 60(sp)
    sw fp, 56(sp)
    addi fp, sp, 64
    sw a0, -12(fp)
    lw t0, -12(fp)
    li t1, 10
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -32(fp)
    lw t0, -32(fp)
    lw t1, -32(fp)
    add t0, t0, t1
    sw t0, -40(fp)
    lw t0, -40(fp)
    sw t0, -28(fp)
    lw t0, -28(fp)
    li t1, 5
    sub t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -24(fp)
    lw t0, -24(fp)
    li t1, 3
    div t0, t0, t1
    sw t0, -48(fp)
    lw t0, -48(fp)
    sw t0, -20(fp)
    lw t0, -20(fp)
    li t1, 7
    rem t0, t0, t1
    sw t0, -52(fp)
    lw t0, -52(fp)
    sw t0, -16(fp)
    lw a0, -12(fp)
    j .L_epilogue_test_multi_dead
.L_epilogue_test_multi_dead:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 64
    ret

    .text
    .globl main
main:
    addi sp, sp, -64
    sw ra, 60(sp)
    sw fp, 56(sp)
    addi fp, sp, 64
    li t0, 0
    sw t0, -12(fp)
    call test_unused_var
    sw a0, -16(fp)
    lw t0, -12(fp)
    lw t1, -16(fp)
    add t0, t0, t1
    sw t0, -20(fp)
    lw t0, -20(fp)
    sw t0, -12(fp)
    li a0, 3
    call test_partial_use
    sw a0, -24(fp)
    lw t0, -12(fp)
    lw t1, -24(fp)
    add t0, t0, t1
    sw t0, -28(fp)
    lw t0, -28(fp)
    sw t0, -12(fp)
    call test_loop_dead
    sw a0, -32(fp)
    lw t0, -12(fp)
    lw t1, -32(fp)
    add t0, t0, t1
    sw t0, -36(fp)
    lw t0, -36(fp)
    sw t0, -12(fp)
    li a0, 10
    call test_if_dead
    sw a0, -40(fp)
    lw t0, -12(fp)
    lw t1, -40(fp)
    add t0, t0, t1
    sw t0, -44(fp)
    lw t0, -44(fp)
    sw t0, -12(fp)
    li a0, 5
    call test_multi_dead
    sw a0, -48(fp)
    lw t0, -12(fp)
    lw t1, -48(fp)
    add t0, t0, t1
    sw t0, -52(fp)
    lw t0, -52(fp)
    sw t0, -12(fp)
    lw a0, -12(fp)
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 64
    ret

