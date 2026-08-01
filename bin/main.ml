(* bin/main.ml *)

(* 检查命令行参数是否包含 -opt *)
let enable_opt () =
  Array.exists ((=) "-opt") Sys.argv

let () =
  try
    (* 1. 从标准输入读取 ToyC 源代码 *)
    let lexbuf = Lexing.from_channel stdin in
    
    (* 2. 词法与语法分析 *)
    let ast = Lib.Parser.prog Lib.Lexer.token lexbuf in
    
    (* 3. 【优化】如果开启 -opt，先做常量折叠 *)
    let ast_optimized =
      if enable_opt () then
        Lib.Ast.fold_const_prog ast
      else
        ast
    in
    
    (* 4. 语义分析与 IR 生成（使用 ast_optimized） *)
    match Lib.Semantic.analyze ast_optimized with
    | Error errors ->
        List.iter
          (fun e -> Printf.fprintf stderr "%s\n" (Lib.Semantic.report_error e))
          errors;
        exit 1

    | Ok ir ->
        (* 直接调用汇编代码生成器 *)
        Lib.Codegen.generate_riscv ir

  with
  | Lib.Lexer.Error msg ->
      Printf.fprintf stderr "Lexical error: %s\n" msg; exit 1
  | Lib.Parser.Error ->
      Printf.fprintf stderr "Parsing error.\n"; exit 1