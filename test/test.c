// test_licm_7.tc
// 测试 if 分支中的不变量
const int N = 10;
const int THRESHOLD = 5;

int main() {
    int sum = 0;
    int i = 0;
    int step = THRESHOLD * 10;  // 50
    
    while (i < N) {
        if (i < THRESHOLD) {
            sum = sum + step;   // step 不变
        } else {
            sum = sum + step * 2;  // step 不变
        }
        i = i + 1;
    }
    return sum;              // 5*50 + 5*100 = 750
}