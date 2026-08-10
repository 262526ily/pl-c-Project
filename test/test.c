// test8_mul_opt.tc
int mul(int a, int b) {
    return a * b;
}

int main() {
    int x = 10;
    int y = 20;
    int z = mul(x, y);   // 200
    return z;
}