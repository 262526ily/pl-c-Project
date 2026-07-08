(* lib/semantic.ml *)

module StringMap = Map.Make(String)

type sym_kind = Var | Const | Func of int

type symbol = {
  name: string;
  kind: sym_kind;
}

type scope = symbol StringMap.t
type symbol_table = scope list

let empty_table () = [StringMap.empty]
let enter_scope tbl = StringMap.empty :: tbl
let exit_scope = function _::rest -> rest | [] -> failwith "empty"

let rec find name = function
  | [] -> None
  | scope::rest -> 
      match StringMap.find_opt name scope with
      | Some s -> Some s | None -> find name rest

let find_current name = function
  | [] -> None
  | scope::_ -> StringMap.find_opt name scope

let add_symbol sym = function
  | [] -> failwith "empty table"
  | scope::rest -> 
      if StringMap.mem sym.name scope then
        failwith ("Redeclaration of '" ^ sym.name ^ "'")
      else
        (StringMap.add sym.name sym scope)::rest

type semantic_error =
  | UndeclaredVar of string
  | Redeclaration of string
  | ConstAssign of string
  | MainNotFound
  | MainWrongReturnType
  | MainWrongParams
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | ArgCountMismatch of string * int * int
  | VoidFuncReturnsValue of string
  | MissingReturn of string        
  | ConstNotCompileTime of string 
  | FuncNotDeclared of string     
  | AssignToFunc of string         

let report_error = function
  | UndeclaredVar s -> "Error: Undeclared identifier '" ^ s ^ "'"
  | Redeclaration s -> "Error: Redeclaration of '" ^ s ^ "'"
  | ConstAssign s -> "Error: Cannot assign to constant '" ^ s ^ "'"
  | MainNotFound -> "Error: 'main' function not found"
  | MainWrongReturnType -> "Error: 'main' must return int"
  | MainWrongParams -> "Error: 'main' must take no parameters"
  | BreakOutsideLoop -> "Error: 'break' outside of loop"
  | ContinueOutsideLoop -> "Error: 'continue' outside of loop"
  | ArgCountMismatch (f, e, a) -> 
      Printf.sprintf "Error: Function '%s' expects %d argument(s), got %d" f e a
  | VoidFuncReturnsValue f -> 
      Printf.sprintf "Error: Void function '%s' cannot return a value" f
  | MissingReturn f -> 
      Printf.sprintf "Error: Function '%s' may not return a value on all paths" f
  | ConstNotCompileTime s -> 
      Printf.sprintf "Error: Constant '%s' initializer is not compile-time constant" s
  | FuncNotDeclared s -> 
      Printf.sprintf "Error: Function '%s' called before declaration" s
  | AssignToFunc s -> 
      Printf.sprintf "Error: Cannot assign to function '%s'" s

type check_state = {
  mutable sym_table: symbol_table;
  mutable in_loop: bool;
  mutable errors: semantic_error list;
  mutable has_main: bool;
  mutable current_func: string option;
  mutable current_func_retty: string option;  
}

let init_state () = {
  sym_table = empty_table ();
  in_loop = false;
  errors = [];
  has_main = false;
  current_func = None;
  current_func_retty = None;
}

let add_err st err = st.errors <- err :: st.errors

(* ============================================================================
   编译期常量检查
   ============================================================================
   常量初始化表达式只能包含：
   - 数字字面量
   - 已声明的常量
   - 由它们组成的算术/逻辑运算
   ========================================================================== *)

let rec is_compile_time_expr st (e: Ast.expr) : bool =
  match e with
  | Ast.EInt _ -> true
  | Ast.EId name ->
      (match find name st.sym_table with
       | Some { kind = Const; _ } -> true
       | _ -> false)
  | Ast.EBinOp (_, e1, e2) ->
      is_compile_time_expr st e1 && is_compile_time_expr st e2
  | Ast.EUnOp (_, e) ->
      is_compile_time_expr st e
  | Ast.ECall _ -> false  

(* ============================================================================
   表达式检查
   ========================================================================== *)

let rec check_expr st (e: Ast.expr) : unit =
  match e with
  | Ast.EInt _ -> ()
  
  | Ast.EId name ->
      (match find name st.sym_table with
       | None -> add_err st (UndeclaredVar name)
       | Some _ -> ())
  
  | Ast.EBinOp (_, e1, e2) ->
      check_expr st e1;
      check_expr st e2
  
  | Ast.EUnOp (_, e) ->
      check_expr st e
  
  | Ast.ECall (fname, args) ->
      (* 函数必须先声明后调用 *)
      (match find fname st.sym_table with
       | None -> 
           add_err st (UndeclaredVar fname)
       | Some sym ->
           (match sym.kind with
            | Func expected_args ->
                let actual_args = List.length args in
                if expected_args <> actual_args then
                  add_err st (ArgCountMismatch (fname, expected_args, actual_args))
            | _ -> 
                add_err st (UndeclaredVar fname));
           List.iter (check_expr st) args)

(* ============================================================================
   声明检查
   ========================================================================== *)

let check_decl st (d: Ast.decl) : unit =
  match d with
  | Ast.VarDecl (name, init) ->
      if find_current name st.sym_table <> None then
        add_err st (Redeclaration name)
      else (
        check_expr st init;
        let sym = { name; kind = Var } in
        st.sym_table <- add_symbol sym st.sym_table
      )
  
  | Ast.ConstDecl (name, init) ->
      if find_current name st.sym_table <> None then
        add_err st (Redeclaration name)
      else (
        (* 常量初始化必须是编译期常量 *)
        if not (is_compile_time_expr st init) then
          add_err st (ConstNotCompileTime name);
        check_expr st init;
        let sym = { name; kind = Const } in
        st.sym_table <- add_symbol sym st.sym_table
      )

(* ============================================================================
   语句检查 + 路径返回分析
   ============================================================================
   返回类型：
   - `true`  : 这条语句保证会执行 return（或无限循环等）
   - `false` : 这条语句不能保证执行 return
   ========================================================================== *)

let rec check_stmt st (s: Ast.stmt) : bool =
  match s with
  | Ast.SBlock stmts ->
      st.sym_table <- enter_scope st.sym_table;
      let returns = check_stmt_list st stmts in
      st.sym_table <- exit_scope st.sym_table;
      returns
  
  | Ast.SEmpty -> false
  
  | Ast.SExpr e -> 
      check_expr st e; 
      false
  
  | Ast.SDecl d -> 
      check_decl st d; 
      false
  
  | Ast.SAssign (name, e) ->
      (match find name st.sym_table with
       | None -> add_err st (UndeclaredVar name)
       | Some sym ->
           if sym.kind = Const then
             add_err st (ConstAssign name)
           else if sym.kind <> Var then
             add_err st (AssignToFunc name);
           check_expr st e);
      false
  
  | Ast.SIf (cond, then_s, else_s) ->
      check_expr st cond;
      let then_returns = check_stmt st then_s in
      let else_returns = 
        match else_s with
        | Some s -> check_stmt st s
        | None -> false
      in
      (* if-else 都保证 return，则整个 if 保证 return *)
      then_returns && else_returns
  
  | Ast.SWhile (cond, body) ->
      check_expr st cond;
      let old_loop = st.in_loop in
      st.in_loop <- true;
      let _ = check_stmt st body in
      st.in_loop <- old_loop;
      false  (* while 不一定执行循环体，所以不保证 return *)
  
  | Ast.SBreak | Ast.SContinue ->
      if not st.in_loop then
        add_err st (if not st.in_loop then BreakOutsideLoop else ContinueOutsideLoop);
      false
  
  | Ast.SReturn e ->
      (match st.current_func_retty, e with
       | Some "void", Some _ ->
           add_err st (VoidFuncReturnsValue (Option.get st.current_func))
       | Some "int", None ->
           add_err st (VoidFuncReturnsValue (Option.get st.current_func))
       | _ -> ());
      Option.iter (check_expr st) e;
      true  (* return 语句保证返回 *)

(* 检查语句列表，返回是否保证 return *)
and check_stmt_list st (stmts: Ast.stmt list) : bool =
  match stmts with
  | [] -> false
  | [s] -> check_stmt st s
  | s::rest ->
      let _ = check_stmt st s in
      check_stmt_list st rest

(* ============================================================================
   顶层单元检查
   ========================================================================== *)

let check_top_unit st (tu: Ast.top_unit) : unit =
  match tu with
  | Ast.UDecl d -> check_decl st d
  
  | Ast.UFunc f ->
      (* 检查 main 函数约束 *)
      if f.Ast.name = "main" then (
        st.has_main <- true;
        if f.Ast.retty <> "int" then
          add_err st MainWrongReturnType;
        if f.Ast.params <> [] then
          add_err st MainWrongParams
      );
      
      (* 注册函数到全局作用域 *)
      let func_sym = { 
        name = f.Ast.name; 
        kind = Func (List.length f.Ast.params) 
      } in
      (match find_current f.Ast.name st.sym_table with
       | Some _ -> add_err st (Redeclaration f.Ast.name)
       | None -> st.sym_table <- add_symbol func_sym st.sym_table);
      
      (* 进入函数作用域 *)
      st.sym_table <- enter_scope st.sym_table;
      let old_func = st.current_func in
      let old_retty = st.current_func_retty in
      st.current_func <- Some f.Ast.name;
      st.current_func_retty <- Some f.Ast.retty;
      
      (* 注册参数为局部变量 *)
      List.iter (fun pname ->
        let sym = { name = pname; kind = Var } in
        st.sym_table <- add_symbol sym st.sym_table
      ) f.Ast.params;
      
      (* 检查函数体，并验证是否所有路径都有 return *)
      let body_returns = check_stmt st f.Ast.body in
      if f.Ast.retty = "int" && not body_returns then
        add_err st (MissingReturn f.Ast.name);
      
      (* 恢复状态 *)
      st.current_func <- old_func;
      st.current_func_retty <- old_retty;
      st.sym_table <- exit_scope st.sym_table

(* ============================================================================
   程序级检查入口
   ============================================================================
   注意：ToyC 要求"函数调用必须写在被调函数声明之后"，
   所以不需要前向声明，按顺序处理即可。
   ========================================================================== *)

let check_program (prog: Ast.prog) : semantic_error list =
  let st = init_state () in
  
  (* 单遍扫描：ToyC 不支持前向调用，按顺序处理即可 *)
  List.iter (check_top_unit st) prog;
  
  if not st.has_main then
    add_err st MainNotFound;
  
  List.rev st.errors

(* ============================================================================
   对外接口：语义分析 + IR 生成
   ========================================================================== *)

let analyze (prog: Ast.prog) : (Ir.ir_program, semantic_error list) result =
  let errs = check_program prog in
  if errs = [] then
    Ok (Ir.generate prog)
  else
    Error errs