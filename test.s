    .text

    .text
    .globl foo
foo:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
entry:
    li t0, 10
    sw t0, -12(fp)
    j .L_epilogue_foo
.L_epilogue_foo:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

    .text
    .globl main
main:
    addi sp, sp, -16
    sw ra, 12(sp)
    sw fp, 8(sp)
    addi fp, sp, 16
entry:
    call foo
    sw a0, -12(fp)
    li a0, 42
    j .L_epilogue_main
.L_epilogue_main:
    lw ra, -4(fp)
    lw fp, -8(fp)
    addi sp, sp, 16
    ret

