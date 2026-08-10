// ============================================================
// 测试程序：验证死代码删除优化
// 
// 包含：
//   1. 未使用的变量赋值（应被删除）
//   2. 未使用的计算结果（应被删除）
//   3. 有用的代码（应保留）
//   4. 条件分支中的死代码
// ============================================================

// ===== 测试 1：未使用的变量 =====
int test_unused_var() {
    int a = 10;          // 赋值后未使用 → 应删除
    int b = 20;          // 赋值后未使用 → 应删除
    int c = a + b;       // 计算结果未使用 → 应删除
    return 100;          // 只有这个有用
}

// ===== 测试 2：部分使用的变量 =====
int test_partial_use(int x) {
    int a = x * 2;       // 下面使用 → 保留
    int b = x * 3;       // 未使用 → 删除
    int c = a + 5;       // 下面使用 → 保留
    return c;            // 返回 c
}

// ===== 测试 3：循环中的死代码 =====
int test_loop_dead() {
    int sum = 0;
    int i = 0;
    int unused = 100;    // 循环外未使用 → 删除
    while (i < 10) {
        int temp = i * 2;    // 未使用 → 应删除
        sum = sum + i;       // 使用 → 保留
        i = i + 1;
    }
    return sum;
}

// ===== 测试 4：条件分支中的死代码 =====
int test_if_dead(int x) {
    int result = 0;
    int a = x * 2;       // 使用 → 保留
    if (x > 5) {
        int b = a * 3;   // 未使用 → 删除
        result = 100;
    } else {
        int c = a + 10;  // 未使用 → 删除
        result = 0;
    }
    int d = result + 1;  // 使用 → 保留
    return d;
}

// ===== 测试 5：多个未使用的计算 =====
int test_multi_dead(int x) {
    int a = x + 10;
    int b = a * 2;
    int c = b - 5;
    int d = c / 3;
    int e = d % 7;
    // 只有 a, b, c, d, e 都未使用 → 全部删除！
    return x;            // 只返回 x
}

// ===== 主函数 =====
int main() {
    int result = 0;
    
    result = result + test_unused_var();      // +100
    result = result + test_partial_use(3);    // 3*2+5 = 11
    result = result + test_loop_dead();       // 0+1+...+9 = 45
    result = result + test_if_dead(10);       // (100+1) = 101
    result = result + test_multi_dead(5);     // +5
    
    return result;  // 100 + 11 + 45 + 101 + 5 = 262
}