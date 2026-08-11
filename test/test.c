// test_const_fold_ops.tc
// 测试各种常量运算的折叠

const int A = 10;
const int B = 20;
const int C = 30;
const int D = 5;

int main() {
    // 1. 算术运算
    int a = A + B;          // 30
    int b = C - A;          // 20
    int c = A * B;          // 200
    int d = C / D;          // 6
    int e = C % A;          // 0
    
    // 2. 混合运算
    int f = (A + B) * 2;    // 60
    int g = (C - A) / D;    // 4
    int h = A * B + C;      // 230
    
    // 3. 逻辑运算
    int i = A > B;          // 0
    int j = A < C;          // 1
    int k = (A == 10) && (B == 20);  // 1
    
    // 4. 复合表达式
    int sum = a + b + c + d + e + f + g + h + i + j + k;
    // 30 + 20 + 200 + 6 + 0 + 60 + 4 + 230 + 0 + 1 + 1 = 552
    
    return sum;
}