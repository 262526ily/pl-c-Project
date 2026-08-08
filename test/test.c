// fib.tc
int fib(int n) {
    int a = 0;
    int b = 1;
    int i = 0;
    int temp = 0;
    while (i < n) {
        temp = a + b;
        a = b;
        b = temp;
        i = i + 1;
    }
    return a;
}

int main() {
    int result = fib(5);
    return result;
}