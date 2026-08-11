// test_licm_3.tc
const int N = 20;
const int A = 5;

int main() {
    int sum = 0;
    int i = 0;
    int step = A * 2;        // 10（不变量）
    
    while (i < N) {
        if (i > 10) {
            sum = sum + step * 2;   // step 不变
        } else {
            sum = sum + step;       // step 不变
        }
        i = i + 1;
    }
    return sum;              // 10*10 + 10*20 = 300
}