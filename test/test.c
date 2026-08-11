// test_licm_large.tc
// 测试循环不变式外提

const int N = 100;
const int A = 10;
const int B = 20;
const int C = 30;

int main() {
    int sum = 0;
    int i = 0;
    int step = A + B;        // 30（循环不变量）
    int limit = N + C;       // 130（循环不变量）
    
    while (i < limit) {
        sum = sum + step;    // step 不变
        i = i + 1;
    }
    return sum;              // 130 * 30 = 3900
}