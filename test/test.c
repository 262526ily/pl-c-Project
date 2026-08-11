// test2_const_prop.tc
const int A = 10;
const int B = 20;
const int C = 30;

int main() {
    int x = A;           // 10
    int y = B;           // 20
    int z = C;           // 30
    int sum = x + y + z; // 60
    return sum;
}