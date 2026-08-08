
// test_mod_large.tc
int mod_large(int x) {
    return x % 4096;
}

int main() {
    int x = 5000;
    int z = mod_large(x);
    return z;
}