// test_dce_simple.tc
// 简化版死代码删除测试

int main() {
    // 这些变量都被使用
    int used1 = 10 + 20;
    int used2 = used1 * 2;
    int used3 = used2 / 5;
    int used4 = used3 - 3;
    int result = used4 + 1;
    
    // 这些变量未被使用 - 应该被删除
    int dead1 = 100;
    int dead2 = 200;
    int dead3 = dead1 + dead2;
    int dead4 = 999;
    int dead5 = 888;
    
    // 被覆盖的赋值 - 第一次赋值应该被删除
    int overwritten = 111;
    overwritten = 222;
    overwritten = 333;
    result = result + overwritten;
    
    // 条件分支中的死代码
    if (1) {
        result = result + 10;
    } else {
        // 以下代码不可达 - 应该被删除
        int unreachable = 444;
        result = result + 555;
    }
    
    // 循环中的未使用变量
    int sum = 0;
    int i = 0;
    while (i < 3) {
        sum = sum + i;
        int loop_dead = i * 10;   // 未使用 - 应删除
        i = i + 1;
    }
    result = result + sum;
    
    // return 之后的死代码
    return result;
    
    // 以下代码永远不可达 - 应该被删除
    int after_return = 666;
    int after_return2 = 777;
    result = result + 888;
}