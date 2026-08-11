// p06_tail_recursion.tc
// 测试尾递归

int sum(int n, int acc) {
    if (n <= 0) {
        return acc;
    } else {
        return sum(n - 1, acc + n);  // 尾递归！
    }
}

int main() {
    return sum(100, 0);  // 5050
}