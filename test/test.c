/// fib_recursive.tc
int fib(int n) {
    int result=0;
    if (n <= 1) {
        result = n;
    } else {
        int n1 = n - 1;
        int n2 = n - 2;
        int f1 = fib(n1);
        int f2 = fib(n2);
        result = f1 + f2;
    }
    return result;
}

int main() {
    int x = fib(8);
    return x;  // 21
}