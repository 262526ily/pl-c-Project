    .text

    .text
    .globl main
main:
    addi sp, sp, -144
    sw ra, 140(sp)
    sw fp, 136(sp)
    addi fp, sp, 144
    li t0, 10
    li t1, 20
    add t0, t0, t1
    sw t0, -80(fp)
    lw t0, -80(fp)
    sw t0, -76(fp)
    lw t0, -76(fp)
    li t1, 2
    li t6, 0
    bge t0, x0, .L_mul_pos1_inline_1
    sub t0, x0, t0
    xori t6, t6, 1
.L_mul_pos1_inline_1:
    bge t1, x0, .L_mul_pos2_inline_2
    sub t1, x0, t1
    xori t6, t6, 1
.L_mul_pos2_inline_2:
    li t2, 0
.L_mul_loop_inline_3:
    beqz t1, .L_mul_end_inline_5
    andi t3, t1, 1
    beqz t3, .L_mul_skip_inline_4
    add t2, t2, t0
.L_mul_skip_inline_4:
    slli t0, t0, 1
    srli t1, t1, 1
    j .L_mul_loop_inline_3
.L_mul_end_inline_5:
    beqz t6, .L_mul_neg_result_inline_6
    sub t2, x0, t2
.L_mul_neg_result_inline_6:
    sw t2, -84(fp)
    lw t0, -84(fp)
    sw t0, -72(fp)
    lw t0, -72(fp)
    li t1, 5
    bnez t1, .L_divmod_start_inline_7
    li t2, 0
    li t3, 0
    j .L_divmod_finish_inline_15
.L_divmod_start_inline_7:
    li t6, 0
    li t4, 0
    bge t0, x0, .L_divmod_n_pos_inline_8
    sub t0, x0, t0
    li t6, 1
    li t4, 1
.L_divmod_n_pos_inline_8:
    bge t1, x0, .L_divmod_d_pos_inline_9
    sub t1, x0, t1
    xori t6, t6, 1
.L_divmod_d_pos_inline_9:
    li t2, 0
    li t3, 0
    li t5, 31
.L_divmod_loop_inline_10:
    bltz t5, .L_divmod_end_inline_12
    slli t3, t3, 1
    srl a2, t0, t5
    andi a2, a2, 1
    or t3, t3, a2
    blt t3, t1, .L_divmod_skip_inline_11
    sub t3, t3, t1
    li a2, 1
    sll a2, a2, t5
    or t2, t2, a2
.L_divmod_skip_inline_11:
    addi t5, t5, -1
    j .L_divmod_loop_inline_10
.L_divmod_end_inline_12:
    beqz t6, .L_divmod_q_pos_inline_13
    sub t2, x0, t2
.L_divmod_q_pos_inline_13:
    beqz t4, .L_divmod_r_pos_inline_14
    sub t3, x0, t3
.L_divmod_r_pos_inline_14:
.L_divmod_finish_inline_15:
    sw t2, -88(fp)
    lw t0, -88(fp)
    sw t0, -68(fp)
    lw t0, -68(fp)
    li t1, 3
    sub t0, t0, t1
    sw t0, -92(fp)
    lw t0, -92(fp)
    sw t0, -64(fp)
    lw t0, -64(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -96(fp)
    lw t0, -96(fp)
    sw t0, -60(fp)
    li t0, 100
    sw t0, -56(fp)
    li t0, 200
    sw t0, -52(fp)
    lw t0, -56(fp)
    lw t1, -52(fp)
    add t0, t0, t1
    sw t0, -100(fp)
    lw t0, -100(fp)
    sw t0, -48(fp)
    li t0, 999
    sw t0, -44(fp)
    li t0, 888
    sw t0, -40(fp)
    li t0, 111
    sw t0, -36(fp)
    li t0, 222
    sw t0, -36(fp)
    li t0, 333
    sw t0, -36(fp)
    lw t0, -60(fp)
    lw t1, -36(fp)
    add t0, t0, t1
    sw t0, -104(fp)
    lw t0, -104(fp)
    sw t0, -60(fp)
    li t0, 1
    beqz t0, L0
    lw t0, -60(fp)
    li t1, 10
    add t0, t0, t1
    sw t0, -108(fp)
    lw t0, -108(fp)
    sw t0, -60(fp)
    j L1
L0:
    li t0, 444
    sw t0, -32(fp)
    lw t0, -60(fp)
    li t1, 555
    add t0, t0, t1
    sw t0, -112(fp)
    lw t0, -112(fp)
    sw t0, -60(fp)
L1:
    li t0, 0
    sw t0, -28(fp)
    li t0, 0
    sw t0, -24(fp)
L2:
    lw t0, -24(fp)
    li t1, 3
    slt t0, t0, t1
    sw t0, -116(fp)
    lw t0, -116(fp)
    beqz t0, L4
L3:
    lw t0, -28(fp)
    lw t1, -24(fp)
    add t0, t0, t1
    sw t0, -120(fp)
    lw t0, -120(fp)
    sw t0, -28(fp)
    lw t0, -24(fp)
    li t1, 10
    li t6, 0
    bge t0, x0, .L_mul_pos1_inline_16
    sub t0, x0, t0
    xori t6, t6, 1
.L_mul_pos1_inline_16:
    bge t1, x0, .L_mul_pos2_inline_17
    sub t1, x0, t1
    xori t6, t6, 1
.L_mul_pos2_inline_17:
    li t2, 0
.L_mul_loop_inline_18:
    beqz t1, .L_mul_end_inline_20
    andi t3, t1, 1
    beqz t3, .L_mul_skip_inline_19
    add t2, t2, t0
.L_mul_skip_inline_19:
    slli t0, t0, 1
    srli t1, t1, 1
    j .L_mul_loop_inline_18
.L_mul_end_inline_20:
    beqz t6, .L_mul_neg_result_inline_21
    sub t2, x0, t2
.L_mul_neg_result_inline_21:
    sw t2, -124(fp)
    lw t0, -124(fp)
    sw t0, -20(fp)
    lw t0, -24(fp)
    li t1, 1
    add t0, t0, t1
    sw t0, -128(fp)
    lw t0, -128(fp)
    sw t0, -24(fp)
    j L2
L4:
    lw t0, -60(fp)
    lw t1, -28(fp)
    add t0, t0, t1
    sw t0, -132(fp)
    lw t0, -132(fp)
    sw t0, -60(fp)
    lw a0, -60(fp)
    j .L_epilogue_main
    li t0, 666
    sw t0, -16(fp)
    li t0, 777
    sw t0, -12(fp)
    lw t0, -60(fp)
    li t1, 888
    add t0, t0, t1
    sw t0, -136(fp)
    lw t0, -136(fp)
    sw t0, -60(fp)
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 144
    ret

