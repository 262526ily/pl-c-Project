(* lib/ir.ml *)

type operand =
  | Const of int
  | Var of string
  | Temp of int

type tac =
  | Assign of operand * operand
  | AssignBinOp of operand * Ast.binop * operand * operand
  | AssignUnOp of operand * Ast.unop * operand
  | Goto of string
  | IfGoto of operand * string
  | IfNotGoto of operand * string
  | Label of string
  | Param of operand
  | Call of operand * string * int
  | Return of operand option

type basic_block = {
  label: string;
  instrs: tac list;
}

type ir_func = {
  fname: string;
  params: string list;
  locals: string list;
  temps: int;
  entry: basic_block;
  blocks: basic_block list;
}

type ir_program_item =
  | GlobalVar of string * int option
  | Function of ir_func

type ir_program = ir_program_item list

type gen = {
  mutable temp_cnt: int;
  mutable label_cnt: int;
  mutable instrs: tac list;
  mutable locals: string list;
}

let new_gen () = { temp_cnt = 0; label_cnt = 0; instrs = []; locals = [] }

let fresh_temp g =
  let t = g.temp_cnt in
  g.temp_cnt <- t + 1;
  Temp t

let fresh_label g =
  let l = g.label_cnt in
  g.label_cnt <- l + 1;
  "L" ^ string_of_int l

let emit g i = g.instrs <- i :: g.instrs
let add_local g name = if not (List.mem name g.locals) then g.locals <- name :: g.locals

(* ============================================================================
   表达式生成（支持短路计算）
   ============================================================================
   && 和 || 需要短路计算：
   - a && b: 若 a 为假，直接返回假，不计算 b
   - a || b: 若 a 为真，直接返回真，不计算 b
   ========================================================================== *)

(* 普通表达式生成 *)
let rec gen_expr g (e: Ast.expr) : operand =
  match e with
  | Ast.EInt n -> Const n
  | Ast.EId name -> add_local g name; Var name
  | Ast.EBinOp (op, e1, e2) ->
      (match op with
       | Ast.And -> gen_short_circuit g e1 e2 true   (* && *)
       | Ast.Or  -> gen_short_circuit g e1 e2 false  (* || *)
       | _ -> gen_normal_binop g op e1 e2)
  | Ast.EUnOp (op, e) ->
      let o = gen_expr g e in
      let result = fresh_temp g in
      emit g (AssignUnOp (result, op, o));
      result
  | Ast.ECall (fname, args) ->
      let arg_ops = List.rev (List.map (gen_expr g) args) in
      List.iter (fun a -> emit g (Param a)) arg_ops;
      let result = fresh_temp g in
      emit g (Call (result, fname, List.length args));
      result

(* 普通二元运算 *)
and gen_normal_binop g op e1 e2 =
  let o1 = gen_expr g e1 in
  let o2 = gen_expr g e2 in
  let result = fresh_temp g in
  emit g (AssignBinOp (result, op, o1, o2));
  result

(* 短路计算生成 *)
and gen_short_circuit g e1 e2 is_and =
  (* 生成：
     t = e1
     if [not] t goto L_short
     t = e2
     goto L_end
   L_short:
     t = [0|1]  -- 短路结果
   L_end:
  *)
  let result = fresh_temp g in
  let short_l = fresh_label g in
  let end_l = fresh_label g in
  
  (* 计算左操作数 *)
  let o1 = gen_expr g e1 in
  emit g (Assign (result, o1));
  
  (* 短路判断 *)
  if is_and then
    emit g (IfNotGoto (result, short_l))  (* &&: 左假则短路 *)
  else
    emit g (IfGoto (result, short_l));     (* ||: 左真则短路 *)
  
  (* 计算右操作数 *)
  let o2 = gen_expr g e2 in
  emit g (Assign (result, o2));
  emit g (Goto end_l);
  
  (* 短路结果 *)
  emit g (Label short_l);
  let short_val = if is_and then Const 0 else Const 1 in
  emit g (Assign (result, short_val));
  
  emit g (Label end_l);
  result

(* ============================================================================
   语句生成
   ========================================================================== *)

type loop_labels = {
  break_l: string;
  continue_l: string;
}

let rec gen_stmt g (loop: loop_labels option) (s: Ast.stmt) : unit =
  match s with
  | Ast.SBlock stmts ->
      List.iter (gen_stmt g loop) stmts
  | Ast.SEmpty -> ()
  | Ast.SExpr e -> 
      let _ = gen_expr g e in ()
  | Ast.SDecl (Ast.VarDecl (name, init)) ->
      add_local g name;
      let t = gen_expr g init in
      emit g (Assign (Var name, t))
  | Ast.SDecl (Ast.ConstDecl (name, init)) ->
      add_local g name;
      let t = gen_expr g init in
      emit g (Assign (Var name, t))
  | Ast.SAssign (name, e) ->
      add_local g name;
      let t = gen_expr g e in
      emit g (Assign (Var name, t))
  | Ast.SIf (cond, then_s, else_s) ->
      let else_l = fresh_label g in
      let end_l = fresh_label g in
      let cond_t = gen_expr g cond in
      emit g (IfNotGoto (cond_t, else_l));
      gen_stmt g loop then_s;
      emit g (Goto end_l);
      emit g (Label else_l);
      Option.iter (gen_stmt g loop) else_s;
      emit g (Label end_l)
  | Ast.SWhile (cond, body) ->
      let cond_l = fresh_label g in
      let body_l = fresh_label g in
      let end_l = fresh_label g in
      let new_loop = { break_l = end_l; continue_l = cond_l } in
      emit g (Goto cond_l);
      emit g (Label body_l);
      gen_stmt g (Some new_loop) body;
      emit g (Label cond_l);
      let cond_t = gen_expr g cond in
      emit g (IfGoto (cond_t, body_l));
      emit g (Label end_l)
  | Ast.SBreak ->
      (match loop with
       | Some l -> emit g (Goto l.break_l)
       | None -> failwith "Break outside loop")
  | Ast.SContinue ->
      (match loop with
       | Some l -> emit g (Goto l.continue_l)
       | None -> failwith "Continue outside loop")
  | Ast.SReturn (Some e) ->
      let t = gen_expr g e in
      emit g (Return (Some t))
  | Ast.SReturn None ->
      emit g (Return None)

(* ============================================================================
   基本块划分
   ========================================================================== *)

let split_blocks (instrs: tac list) : basic_block list =
  let rec split current_label current acc = function
    | [] ->
        let block = { label = current_label; instrs = List.rev current } in
        List.rev (block :: acc)
    | (Label l) :: rest ->
        let block = { label = current_label; instrs = List.rev current } in
        split l [] (block :: acc) rest
    | i :: rest ->
        split current_label (i :: current) acc rest
  in
  match instrs with
  | (Label l) :: rest -> split l [] [] rest
  | _ -> split "entry" [] [] instrs

(* ============================================================================
   函数生成
   ========================================================================== *)

let gen_func (f: Ast.func_def) : ir_func =
  let g = new_gen () in
  List.iter (add_local g) f.Ast.params;
  gen_stmt g None f.Ast.body;
  let all_instrs = List.rev g.instrs in
  let blocks = split_blocks all_instrs in
  let entry = List.hd blocks in
  let rest = List.tl blocks in
  {
    fname = f.Ast.name;
    params = f.Ast.params;
    locals = g.locals;
    temps = g.temp_cnt;
    entry;
    blocks = rest;
  }

(* ============================================================================
   程序生成
   ========================================================================== *)

let generate (prog: Ast.prog) : ir_program =
  List.filter_map (function
    | Ast.UFunc f -> Some (Function (gen_func f))
    | Ast.UDecl (Ast.VarDecl (name, init)) -> 
        let v = match init with Ast.EInt n -> Some n | _ -> None in
        Some (GlobalVar (name, v))
    | Ast.UDecl (Ast.ConstDecl (name, init)) ->
        let v = match init with Ast.EInt n -> Some n | _ -> None in
        Some (GlobalVar (name, v))
  ) prog

(* ============================================================================
   打印
   ========================================================================== *)

let op_str = function
  | Const n -> string_of_int n
  | Var s -> s
  | Temp n -> "t" ^ string_of_int n

let binop_str = Ast.(function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Ne -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
  | And -> "&&" | Or -> "||"
)

let unop_str = Ast.(function
  | Pos -> "+" | Neg -> "-" | Not -> "!"
)

let tac_str = function
  | Assign (x, y) -> Printf.sprintf "%s = %s" (op_str x) (op_str y)
  | AssignBinOp (x, op, y, z) ->
      Printf.sprintf "%s = %s %s %s" (op_str x) (op_str y) (binop_str op) (op_str z)
  | AssignUnOp (x, op, y) ->
      Printf.sprintf "%s = %s%s" (op_str x) (unop_str op) (op_str y)
  | Goto l -> "goto " ^ l
  | IfGoto (x, l) -> Printf.sprintf "if %s goto %s" (op_str x) l
  | IfNotGoto (x, l) -> Printf.sprintf "ifFalse %s goto %s" (op_str x) l
  | Label l -> l ^ ":"
  | Param x -> "param " ^ op_str x
  | Call (x, f, n) -> Printf.sprintf "%s = call %s, %d" (op_str x) f n
  | Return (Some x) -> "return " ^ op_str x
  | Return None -> "return"

let dump_block b =
  Printf.printf "%s:\n" b.label;
  List.iter (fun i -> Printf.printf "  %s\n" (tac_str i)) b.instrs

let dump_func f =
  Printf.printf "\nfunc %s(%s):\n" f.fname (String.concat ", " f.params);
  Printf.printf "  locals: [%s]\n" (String.concat ", " f.locals);
  Printf.printf "  temps: %d\n\n" f.temps;
  dump_block f.entry;
  List.iter dump_block f.blocks

let dump_ir prog =
  List.iter (function
    | GlobalVar (name, Some v) -> Printf.printf "global %s = %d\n" name v
    | GlobalVar (name, None) -> Printf.printf "global %s\n" name
    | Function f -> dump_func f
  ) prog