(* main.ml *)
let () =
  (* 解析命令行参数 *)
  let optimize = ref false in
  let args = Array.to_list Sys.argv in
  List.iter (fun arg ->
    if arg = "-opt" then (
      optimize := true;
      (* Printf.eprintf "=== Optimization ENABLED ===\n" *)
    )
  ) args;
  
  try
    (* 1. 从标准输入读取 ToyC 源代码 *)
    let lexbuf = Lexing.from_channel stdin in
    
    (* 2. 词法与语法分析 *)
    let ast = Lib.Parser.prog Lib.Lexer.token lexbuf in
    
    (* 3. 语义分析与 IR 生成（带优化选项） *)
    (match Lib.Semantic.analyze_with_opt ast !optimize with
    | Error errors ->
        List.iter
          (fun e -> Printf.fprintf stderr "%s\n" (Lib.Semantic.report_error e))
          errors;
        exit 1

    | Ok ir ->
        (* 如果优化启用，打印优化后的IR到stderr用于调试 *)
        (* if !optimize then (
          Printf.eprintf "\n=== Optimized IR ===\n";
          Lib.Ir.dump_ir ir
        ); *)
        (* 直接调用汇编代码生成器 *)
        Lib.Codegen.generate_riscv ir
    );
  
  with
  | Lib.Lexer.Error msg ->
      Printf.fprintf stderr "Lexical error: %s\n" msg; exit 1
  | Lib.Parser.Error ->
      Printf.fprintf stderr "Parsing error.\n"; exit 1