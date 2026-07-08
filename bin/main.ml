(* 
   编译器主入口点：
   功能：从标准输入读取 ToyC 源代码，调用 Lexer 和 Parser 生成 AST，
   最后打印 AST 并输出结果。
*)
let () =
  try
    (* 1. 创建 Lexing 缓冲流（根据 PDF 要求从 stdin 读取） *)
    let lexbuf = Lexing.from_channel stdin in
    
    (* 2. 调用语法分析器入口：Parser.prog *)
    let ast = Lib.Parser.prog Lib.Lexer.token lexbuf in
    
    (* 3. 调试输出：打印AST *)
    Lib.Ast.dump_ast ast;

    (* 4. 语义分析 + IR生成 *)
    (match Lib.Semantic.analyze ast with
    | Error errors ->
        List.iter
          (fun e ->
              Printf.printf "%s\n"
                (Lib.Semantic.report_error e))
          errors;
        exit 1

    | Ok ir ->
        Printf.printf "Semantic check success!\n";
        Lib.Ir.dump_ir ir
    );

    (* 5. 打印统计信息 *)
    Printf.printf "Success: Units parsed: %d\n" (List.length ast)

  with
  | Lib.Lexer.Error msg ->
      (* 词法阶段错误处理 *)
      Printf.fprintf stderr "Lexical error: %s\n" msg;
      exit 1
  | Lib.Parser.Error ->
      (* 语法阶段错误处理 *)
      Printf.fprintf stderr "Syntax error.\n";
      exit 1