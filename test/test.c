// // test_const_fold.tc
// 测试常量折叠是否被寄存器分配影响

const int A = 10;
const int B = 20;
const int C = 30;

int main() {
    int x = A + B;       // 10+20=30
    int y = C - A;       // 30-10=20
    int z = x * y;       // 30*20=600
    int w = z / 3;       // 600/3=200
    int v = w % 7;       // 200%7=4
    return v + 2;        // 4+2=6
}