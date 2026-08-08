
<<<<<<< HEAD
// test_mod_large.tc
int mod_large(int x) {
    return x % 4096;
}

int main() {
    int x = 5000;
    int z = mod_large(x);
=======
// test_runtime_mul.tc
int mul(int a, int b) {
    return a * b;
}

int main() {
    int x = 10;
    int y = 20;
    int z = mul(x, y);
>>>>>>> 3d3e7f4 (内联乘法优化)
    return z;
}