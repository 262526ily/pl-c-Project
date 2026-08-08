
// ============================================================
// 测试：同时检测小掩码和大掩码取模优化
// ============================================================

// 小掩码 (<= 2047)：应该用 andi
int mod_small(int x) {
    return x % 64;        // mask = 63
}

// 大掩码 (> 2047)：应该用 li + and
int mod_large(int x) {
    return x % 4096;      // mask = 4095
}

// 运行时取模：应该用 rem
int mod_runtime(int a, int b) {
    return a % b;
}

int main() {
    int x = 5000;
    int a = mod_small(x);     // 5000 % 64 = 8
    int b = mod_large(x);     // 5000 % 4096 = 904
    int c = mod_runtime(x, 7); // 5000 % 7 = 2
    return a + b + c;         // 8 + 904 + 2 = 914
}