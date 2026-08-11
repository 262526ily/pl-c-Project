// test_void.tc
// 测试 void 函数的 return

void foo() {
    int x = 10;
    return;          // void return
}

int main() {
    foo();
    return 42;
}