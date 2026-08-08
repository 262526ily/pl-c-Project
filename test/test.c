// test_const_fold.tc
const int A = 10;
const int B = 20;

int main() {
    int x = A + B;
    int y = x * 2;
    int z = y - 30;
    int w = z / 3;
    return w;
}