// test_if_const.tc
// 测试常量条件分支

const int A = 10;
const int B = 5;

int main() {
    int x = A;           // x = 10
    int y = B;           // y = 5
    
    if (x > y) {         // 10 > 5 = true
        return 100;
    } else {
        return 200;
    }
}