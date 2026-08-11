// test_if_false.tc
// 测试条件为假时的死代码删除

const int A = 10;
const int B = 5;

int main() {
    int x = A;           // 10
    int y = B;           // 5
    
    if (x < y) {         // 10 < 5 = false
        return x;        // 永远不会执行
    } else {
        return y;        // 执行这个
    }
}