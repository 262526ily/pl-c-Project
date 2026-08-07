
func main():
  locals: [i$13, sum$12, result$4]
  temps: 15

entry:
  goto L1
L0:
  t8 = result$4 + 555
  result$4 = t8
L1:
L2:
  t9 = i$13 < 3
  ifFalse t9 goto L4
L3:
  t10 = sum$12 + i$13
  sum$12 = t10
  t11 = i$13 * 10
  t12 = i$13 + 1
  i$13 = t12
  goto L2
L4:
  t13 = result$4 + sum$12
  result$4 = t13
  return result$4
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
    li t1, 10
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
    sw t2, -68(fp)
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

