// test_loop_cse.tc
// 测试循环中的公共子表达式消除

int main() {
    int sum = 0;
    int i = 0;
    while (i < 100) {
        int a = i * 2;      // 计算1
        int b = i * 2;      // 重复，CSE 应消除
        int c = i * 2;      // 重复，CSE 应消除
        sum = sum + a + b + c;
        i = i + 1;
    }
    return sum;
}