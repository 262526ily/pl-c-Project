我完成了什么 (What I've done)
作为成员 B，我已完成语义分析与中间代码生成（IR）阶段：

1. 符号表与作用域管理 (lib/semantic.ml)：使用 OCaml Map 模块实现作用域栈，支持变量、常量和函数在多级嵌套作用域中的声明与查询。内层作用域可屏蔽外层同名标识符。

2. 语义检查 (lib/semantic.ml)：遍历 AST 完成全部语义约束验证，包括：
   - 变量/常量先声明后使用；
   - main 函数存在性、返回类型为 int、参数列表为空；
   - 函数调用时实参与形参个数匹配；
   - const 常量不可被赋值；
   - break/continue 必须位于循环体内；
   - const 初始化表达式必须是编译期可确定的常量；
   - int 函数的所有执行路径必须包含 return 语句。

3. 中间表示生成 (lib/ir.ml)：将语义检查通过的 AST 转换为三地址码（Three-Address Code）。设计了包含赋值、二元/一元运算、条件/无条件跳转、函数调用（param + call）、返回等指令的 IR，支持 && 和 || 的短路计算，并完成基本块划分。

4. 入口程序更新 (bin/main.ml)：在成员 A 的解析流程后接入语义分析与 IR 生成，输出语义检查结果及三地址码。

5. 测试脚本 (test_semantic_ir.sh)：编写批量测试脚本，覆盖 18 个功能测试、16 个语义错误测试和 5 个边界测试，共 39 个用例全部通过。

如何验证 (How to test)
合并后，大家可以在终端运行以下命令验证：

Bash
dune build
./test_semantic_ir.sh

如果看到所有测试显示 ✓ PASS，说明语义分析与 IR 生成阶段工作正常。也可手动测试：

Bash
echo 'int main() { int a = 1; return a; }' | dune exec bin/main.exe

应输出 --- AST DUMP ---、--- SEMANTIC CHECK PASSED --- 及三地址码 IR。

给成员 C/D（代码生成）的指引 (Instructions for Member C/D)
前端解析与语义分析已跑通，你可以开始编写代码生成模块了，以下是我的建议：

1. 数据入口：语义分析的输出类型是 (Ir.ir_program, Semantic.semantic_error list) result。你的任务是处理 Ok 分支中的 Ir.ir_program，将其转换为 RISC-V32 汇编代码。

2. 关注全局变量：Ir.ir_program 中包含 GlobalVar 项（全局变量/常量）和 Function 项。全局变量需要生成 .data 段，函数生成 .text 段。

3. 函数调用约定：IR 中的 Call 指令使用 param 压栈方式传递参数。你需要按照 RISC-V 调用约定（a0-a7 寄存器传参，栈对齐等）生成对应的汇编指令。

4. 临时变量分配：IR 中的 Temp 临时变量需要映射到寄存器或栈帧位置。建议先实现简单的栈分配，再考虑寄存器分配优化。

5. 调试建议：你可以随时调用 Lib.Ir.dump_ir 来查看当前函数的三地址码，确认你的代码生成逻辑走到了哪个基本块。
