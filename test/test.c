// test_licm_2.tc
const int A = 10;
const int B = 20;
const int C = 30;
const int N = 50;

int main() {
    int sum = 0;
    int i = 0;
    int step = A + B;        // 30（不变量）
    int limit = N + C;       // 80（不变量）
    
    while (i < limit) {
        sum = sum + step;
        i = i + 1;
    }
    return sum;              // 80 * 30 = 2400
}