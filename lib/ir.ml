(* lib/ir.ml
 * 
 * 实现了一个编译器中间表示（IR）生成器。
 * 将抽象语法树（AST）转换为三地址码（TAC）形式的中级中间表示，
 * 并支持基本块划分、短路求值、循环控制流（break/continue）等功能。
 * 最后提供了一组打印函数，用于输出 IR 的可读形式。
 *)

 module StringSet = Set.Make(String)

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
   | Empty  (* 用于优化时删除指令 *)
 
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
   mutable instrs: tac list;
   mutable locals: string list;
   mutable unique_cnt: int;
   mutable scopes: (string * string) list list;
 }
 
 (* 创建一个新的 IR 生成器状态，初始化临时变量和标签计数器 *)
 let new_gen () = { temp_cnt = 0; instrs = []; locals = []; unique_cnt = 0; scopes = [] }
 
 (* 生成一个新的临时变量（Temp n），并递增计数器 *)
 let fresh_temp g =
   let t = g.temp_cnt in
   g.temp_cnt <- t + 1;
   Temp t
 
 (* 生成一个新的标签（如 "L0"），并递增计数器 *)
 let global_label_cnt = ref 0
 
 let fresh_label () =
   let l = !global_label_cnt in
   global_label_cnt := l + 1;
   "L" ^ string_of_int l
 
 (* 向当前生成器的指令列表中添加一条 TAC 指令 *)
 let emit g i = g.instrs <- i :: g.instrs
 
 (* 将变量名加入当前函数的局部变量列表（去重） *)
 let add_local g name = if not (List.mem name g.locals) then g.locals <- name :: g.locals
 
 
 (* 作用域感知的局部变量唯一命名：
    同名变量在不同作用域中会生成不同的 IR 名字（如 x$0、x$1），
    避免被遮蔽的变量在代码生成时塌缩到同一个栈槽。 *)
 
 (* 进入新的局部作用域（对应一个花括号块或函数参数区） *)
 let enter_scope g = g.scopes <- [] :: g.scopes
 
 (* 退出当前作用域 *)
 let exit_scope g =
   match g.scopes with
   | _ :: rest -> g.scopes <- rest
   | [] -> failwith "scope underflow"
 
 (* 从内向外查找名字的局部重命名 *)
 let rec find_binding name = function
   | [] -> None
   | scope :: rest ->
       (match List.assoc_opt name scope with
        | Some u -> Some u
        | None -> find_binding name rest)
 
 (* 解析变量名：局部变量返回其唯一名；未找到则视为全局符号，保持原名 *)
 let resolve_name g name =
   match find_binding name g.scopes with
   | Some u -> u
   | None -> name
 
 (* 为当前作用域的新声明分配唯一名（$ 不会出现在用户标识符中，避免冲突） *)
 let bind g name =
   let u = Printf.sprintf "%s$%d" name g.unique_cnt in
   g.unique_cnt <- g.unique_cnt + 1;
   (match g.scopes with
    | scope :: rest -> g.scopes <- ((name, u) :: scope) :: rest
    | [] -> g.scopes <- [[(name, u)]]);
   add_local g u;
   u
 
 
 (* 表达式生成（支持短路计算） *)
 
 (* 将 AST 表达式转换为 TAC 操作数，生成对应的中间代码 *)
 let rec gen_expr g (e: Ast.expr) : operand =
   match e with
   | Ast.EInt n -> Const n
   | Ast.EId name -> Var (resolve_name g name)
   | Ast.EBinOp (op, e1, e2) ->
       (match op with
        | Ast.And -> gen_short_circuit g e1 e2 true
        | Ast.Or  -> gen_short_circuit g e1 e2 false
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
 
 (* 生成普通二元运算的 TAC 代码（非短路运算） *)
 and gen_normal_binop g op e1 e2 =
   let o1 = gen_expr g e1 in
   let o2 = gen_expr g e2 in
   let result = fresh_temp g in
   emit g (AssignBinOp (result, op, o1, o2));
   result
 
 (* 生成逻辑与/或的短路求值 TAC 代码 *)
 and gen_short_circuit g e1 e2 is_and =
   let result = fresh_temp g in
   let short_l = fresh_label () in
   let end_l = fresh_label () in
 
   let o1 = gen_expr g e1 in
   emit g (Assign (result, o1));
 
   if is_and then
     emit g (IfNotGoto (result, short_l))
   else
     emit g (IfGoto (result, short_l));
 
   let o2 = gen_expr g e2 in
   emit g (Assign (result, o2));
   emit g (Goto end_l);
 
   emit g (Label short_l);
   let short_val = if is_and then Const 0 else Const 1 in
   emit g (Assign (result, short_val));
 
   emit g (Label end_l);
   result
 
 
 (* 语句生成 *)
 
 type loop_labels = {
   break_l: string;
   continue_l: string;
 }
 
 (* 将 AST 语句转换为 TAC 代码，支持 break/continue 和循环结构 *)
 let rec gen_stmt g (loop: loop_labels option) (s: Ast.stmt) : unit =
   match s with
   | Ast.SBlock stmts ->
       enter_scope g;
       List.iter (gen_stmt g loop) stmts;
       exit_scope g
   | Ast.SEmpty -> ()
   | Ast.SExpr e -> 
       let _ = gen_expr g e in ()
   | Ast.SDecl (Ast.VarDecl (name, init)) ->
       let t = gen_expr g init in
       let u = bind g name in
       emit g (Assign (Var u, t))
   | Ast.SDecl (Ast.ConstDecl (name, init)) ->
       let t = gen_expr g init in
       let u = bind g name in
       emit g (Assign (Var u, t))
   | Ast.SAssign (name, e) ->
       let t = gen_expr g e in
       emit g (Assign (Var (resolve_name g name), t))
   | Ast.SIf (cond, then_s, else_s) ->
       let else_l = fresh_label () in
       let end_l = fresh_label () in
       let cond_t = gen_expr g cond in
       emit g (IfNotGoto (cond_t, else_l));
       gen_stmt g loop then_s;
       emit g (Goto end_l);
       emit g (Label else_l);
       Option.iter (gen_stmt g loop) else_s;
       emit g (Label end_l)
   | Ast.SWhile (cond, body) ->
       let cond_l = fresh_label () in
       let body_l = fresh_label () in
       let end_l = fresh_label () in
       let new_loop = { break_l = end_l; continue_l = cond_l } in
 
       
       emit g (Label cond_l);
       let cond_t = gen_expr g cond in
       emit g (IfNotGoto (cond_t, end_l));
 
       emit g (Label body_l);
       gen_stmt g (Some new_loop) body;
       emit g (Goto cond_l);
 
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
 
 
 (* 基本块划分 *)
 
 (* 将 TAC 指令列表按 Label 划分为基本块列表 *)
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
 
 
 (* 函数生成 *)
 
 (* 将 AST 函数定义转换为完整的 IR 函数（含基本块划分） *)
 let gen_func (f: Ast.func_def) : ir_func =
   let g = new_gen () in
   enter_scope g;
   let param_names = List.map (fun p -> bind g p) f.Ast.params in
   gen_stmt g None f.Ast.body;
   exit_scope g;
 
   (* FIX: 确保 void 函数有 return，避免空指令序列 *)
   (match f.Ast.retty with
    | "void" -> 
        if g.instrs = [] || 
           (match List.hd g.instrs with Return _ -> false | _ -> true) then
          emit g (Return None)
    | _ -> ());
 
   let all_instrs = List.rev g.instrs in
   let blocks = split_blocks all_instrs in
 
   (* FIX: 处理空块情况 *)
   match blocks with
   | [] ->
       { fname = f.Ast.name;
         params = param_names;
         locals = g.locals;
         temps = g.temp_cnt;
         entry = { label = "entry"; instrs = [] };
         blocks = [] }
   | entry :: rest ->
       { fname = f.Ast.name;
         params = param_names;
         locals = g.locals;
         temps = g.temp_cnt;
         entry;
         blocks = rest }
 
 
 (* 程序生成 *)
 
 (* 编译期常量求值：用于计算全局变量/常量的静态初值（含常量链、算术、比较等） *)
 let eval_binop = Ast.(function
   | Add -> ( + )
   | Sub -> ( - )
   | Mul -> ( * )
   | Div -> ( / )
   | Mod -> ( mod )
   | Eq -> (fun a b -> if a = b then 1 else 0)
   | Ne -> (fun a b -> if a <> b then 1 else 0)
   | Lt -> (fun a b -> if a < b then 1 else 0)
   | Gt -> (fun a b -> if a > b then 1 else 0)
   | Le -> (fun a b -> if a <= b then 1 else 0)
   | Ge -> (fun a b -> if a >= b then 1 else 0)
   | And -> (fun a b -> if a <> 0 && b <> 0 then 1 else 0)
   | Or -> (fun a b -> if a <> 0 || b <> 0 then 1 else 0))
 
 (* 在给定全局环境中求值表达式；无法确定时返回 None *)
 let rec eval_const (env: (string * int) list) (e: Ast.expr) : int option =
   match e with
   | Ast.EInt n -> Some n
   | Ast.EId name -> List.assoc_opt name env
   | Ast.EBinOp (op, e1, e2) ->
       (* 对 && / || 做短路求值，避免 0 && (1/0) 之类的除零 *)
       (match op, eval_const env e1 with
        | Ast.And, Some 0 -> Some 0
        | Ast.Or, Some n when n <> 0 -> Some 1
        | _, Some a ->
            (match eval_const env e2 with
             | Some b ->
                 (match op with
                  | Ast.Div | Ast.Mod when b = 0 -> None
                  | _ -> Some (eval_binop op a b))
             | None -> None)
        | _, None -> None)
   | Ast.EUnOp (op, e) ->
       (match eval_const env e with
        | Some n ->
            (match op with
             | Ast.Pos -> Some n
             | Ast.Neg -> Some (-n)
             | Ast.Not -> Some (if n = 0 then 1 else 0))
        | None -> None)
   | Ast.ECall _ -> None
 
 (* 将 AST 程序（函数和全局变量声明列表）转换为 IR 程序 *)
 let generate (prog: Ast.prog) : ir_program =
   (* 按声明顺序累积的全局常量/变量求值环境 *)
   let env = ref [] in
   List.filter_map (function
     | Ast.UFunc f -> Some (Function (gen_func f))
     | Ast.UDecl (Ast.VarDecl (name, init)) -> 
         let v = eval_const !env init in
         (match v with Some n -> env := (name, n) :: !env | None -> ());
         Some (GlobalVar (name, v))
     | Ast.UDecl (Ast.ConstDecl (name, init)) ->
         let v = eval_const !env init in
         (match v with Some n -> env := (name, n) :: !env | None -> ());
         Some (GlobalVar (name, v))
   ) prog
 
 
 (* ================================================================
    常量折叠与传播优化
    ================================================================ *)
 
 (* 常量值类型 *)
 type const_val = 
   | ConstInt of int
   | Unknown
 
 (* 常量环境 *)
 module ConstEnv = Map.Make(String)
 
 (* 从操作数获取常量值 *)
 let operand_to_const env = function
   | Const n -> Some n
   | Var name ->
       begin try 
         match ConstEnv.find name env with
         | ConstInt n -> Some n
         | Unknown -> None
       with Not_found -> None
       end
   | Temp t ->
       begin try
         let key = Printf.sprintf "t%d" t in
         match ConstEnv.find key env with
         | ConstInt n -> Some n
         | Unknown -> None
       with Not_found -> None
       end
 
 (* 操作数转环境键 *)
 let operand_key = function
   | Const n -> Printf.sprintf "imm_%d" n
   | Var s -> s
   | Temp t -> Printf.sprintf "t%d" t
 
 (* 二元运算常量求值 *)
 let eval_const_binop op a b =
   match op with
   | Ast.Add -> Some (a + b)
   | Ast.Sub -> Some (a - b)
   | Ast.Mul -> Some (a * b)
   | Ast.Div -> if b <> 0 then Some (a / b) else None
   | Ast.Mod -> if b <> 0 then Some (a mod b) else None
   | Ast.Eq -> Some (if a = b then 1 else 0)
   | Ast.Ne -> Some (if a <> b then 1 else 0)
   | Ast.Lt -> Some (if a < b then 1 else 0)
   | Ast.Gt -> Some (if a > b then 1 else 0)
   | Ast.Le -> Some (if a <= b then 1 else 0)
   | Ast.Ge -> Some (if a >= b then 1 else 0)
   | Ast.And -> Some (if a <> 0 && b <> 0 then 1 else 0)
   | Ast.Or -> Some (if a <> 0 || b <> 0 then 1 else 0)
 
 (* 一元运算常量求值 *)
 let eval_const_unop op a =
   match op with
   | Ast.Pos -> Some a
   | Ast.Neg -> Some (-a)
   | Ast.Not -> Some (if a = 0 then 1 else 0)
 
 (* 对单条TAC指令进行常量折叠 *)
 let fold_const_instr env instr =
   match instr with
   (* 赋值指令 *)
   | Assign (dest, src) ->
       let dest_key = operand_key dest in
       begin
         match operand_to_const env src with
         | Some n ->
             (* 源是常量，直接折叠 *)
             (ConstEnv.add dest_key (ConstInt n) env, Assign (dest, Const n))
         | None ->
             (ConstEnv.add dest_key Unknown env, instr)
       end
 
   (* 二元运算 *)
   | AssignBinOp (dest, op, src1, src2) ->
       let dest_key = operand_key dest in
       begin
         match operand_to_const env src1, operand_to_const env src2 with
         | Some a, Some b ->
             (* 两个操作数都是常量，直接折叠 *)
             begin
               match eval_const_binop op a b with
               | Some n ->
                   (ConstEnv.add dest_key (ConstInt n) env, Assign (dest, Const n))
               | None ->
                   (ConstEnv.add dest_key Unknown env, instr)
             end
         | _, _ ->
             (* 尝试恒等式优化 *)
             let optimized =
               match op, src1, src2 with
               (* x + 0 = x *)
               | Ast.Add, src, Const 0 -> Some (Assign (dest, src))
               (* 0 + x = x *)
               | Ast.Add, Const 0, src -> Some (Assign (dest, src))
               (* x - 0 = x *)
               | Ast.Sub, src, Const 0 -> Some (Assign (dest, src))
               (* x * 1 = x *)
               | Ast.Mul, src, Const 1 -> Some (Assign (dest, src))
               (* 1 * x = x *)
               | Ast.Mul, Const 1, src -> Some (Assign (dest, src))
               (* x * 0 = 0 *)
               | Ast.Mul, _, Const 0 -> Some (Assign (dest, Const 0))
               (* 0 * x = 0 *)
               | Ast.Mul, Const 0, _ -> Some (Assign (dest, Const 0))
               (* x / 1 = x *)
               | Ast.Div, src, Const 1 -> Some (Assign (dest, src))
               (* x % 1 = 0 *)
               | Ast.Mod, _, Const 1 -> Some (Assign (dest, Const 0))
               (* x == x = 1 *)
               | Ast.Eq, src1, src2 when src1 = src2 -> Some (Assign (dest, Const 1))
               (* x != x = 0 *)
               | Ast.Ne, src1, src2 when src1 = src2 -> Some (Assign (dest, Const 0))
               (* 逻辑与短路 *)
               | Ast.And, Const 0, _ -> Some (Assign (dest, Const 0))
               | Ast.And, _, Const 0 -> Some (Assign (dest, Const 0))
               | Ast.And, Const n, src when n <> 0 -> Some (Assign (dest, src))
               | Ast.And, src, Const n when n <> 0 -> Some (Assign (dest, src))
               (* 逻辑或短路 *)
               | Ast.Or, Const 1, _ -> Some (Assign (dest, Const 1))
               | Ast.Or, _, Const 1 -> Some (Assign (dest, Const 1))
               | Ast.Or, Const 0, src -> Some (Assign (dest, src))
               | Ast.Or, src, Const 0 -> Some (Assign (dest, src))
               | _ -> None
             in
             begin
               match optimized with
               | Some new_instr ->
                   (* 检查优化后的指令是否也是常量 *)
                   begin
                     match new_instr with
                     | Assign (_, src) ->
                         begin
                           match operand_to_const env src with
                           | Some n ->
                               (ConstEnv.add dest_key (ConstInt n) env, Assign (dest, Const n))
                           | None ->
                               (ConstEnv.add dest_key Unknown env, new_instr)
                         end
                     | _ ->
                         (ConstEnv.add dest_key Unknown env, new_instr)
                   end
               | None ->
                   (ConstEnv.add dest_key Unknown env, instr)
             end
       end
 
   (* 一元运算 *)
   | AssignUnOp (dest, op, src) ->
       let dest_key = operand_key dest in
       begin
         match operand_to_const env src with
         | Some n ->
             begin
               match eval_const_unop op n with
               | Some n' ->
                   (ConstEnv.add dest_key (ConstInt n') env, Assign (dest, Const n'))
               | None ->
                   (ConstEnv.add dest_key Unknown env, instr)
             end
         | None ->
             (ConstEnv.add dest_key Unknown env, instr)
       end
 
   (* 条件跳转：常量条件可以简化 *)
   | IfGoto (cond, label) ->
       begin
         match operand_to_const env cond with
         | Some n ->
             if n = 0 then
               (* 条件为假，删除跳转 *)
               (env, Empty)
             else
               (* 条件为真，替换为无条件跳转 *)
               (env, Goto label)
         | None ->
             (env, instr)
       end
 
   | IfNotGoto (cond, label) ->
       begin
         match operand_to_const env cond with
         | Some n ->
             if n <> 0 then
               (* 条件为真，删除跳转 *)
               (env, Empty)
             else
               (* 条件为假，替换为无条件跳转 *)
               (env, Goto label)
         | None ->
             (env, instr)
       end
 
   (* 其他指令保持不变 *)
   | _ -> (env, instr)
 
 (* 对整个指令列表进行常量折叠 *)
 let fold_constants (instrs: tac list) : tac list =
   let env = ref ConstEnv.empty in
   let result = ref [] in
   
   List.iter (fun instr ->
     let new_env, new_instr = fold_const_instr !env instr in
     env := new_env;
     if new_instr <> Empty then
       result := new_instr :: !result
   ) instrs;
   
   List.rev !result
 
 (* 对基本块进行常量折叠 *)
 let fold_constants_block (block: basic_block) : basic_block =
   { block with instrs = fold_constants block.instrs }
 
 
 (* ================================================================
    死代码删除优化
    ================================================================ *)
 
 (* 收集指令中使用的变量 - 返回 string list *)
 let collect_used_vars instrs =
   let used = ref [] in
   
   let add_used v =
     if not (List.mem v !used) then
       used := v :: !used
   in
   
   let mark_used = function
     | Const _ -> ()
     | Var v -> add_used v
     | Temp t -> add_used (Printf.sprintf "t%d" t)
   in
   
   List.iter (fun instr ->
     match instr with
     | Assign (_, src) -> mark_used src
     | AssignBinOp (_, _, src1, src2) -> mark_used src1; mark_used src2
     | AssignUnOp (_, _, src) -> mark_used src
     | IfGoto (cond, _) -> mark_used cond
     | IfNotGoto (cond, _) -> mark_used cond
     | Param x -> mark_used x
     | Call (dest, _, _) -> mark_used dest
     | Return (Some x) -> mark_used x
     | Goto _ | Label _ | Return None -> ()
     | Empty -> ()
   ) instrs;
   
   !used
 
 (* 判断指令是否有副作用（必须保留） *)
 let has_side_effect = function
   | Call _ -> true
   | Return _ -> true
   | Goto _ -> true
   | IfGoto _ -> true
   | IfNotGoto _ -> true
   | Label _ -> true
   | Param _ -> true
   | Assign _ -> false
   | AssignBinOp _ -> false
   | AssignUnOp _ -> false
   | Empty -> false
 
 (* 获取指令定义的目标变量 *)
 let defined_var = function
   | Assign (dest, _) -> Some (operand_key dest)
   | AssignBinOp (dest, _, _, _) -> Some (operand_key dest)
   | AssignUnOp (dest, _, _) -> Some (operand_key dest)
   | Call (dest, _, _) -> Some (operand_key dest)
   | _ -> None
 
 (* 收集所有跳转目标标签 *)
 let collect_used_labels instrs =
   let used = ref [] in
   let add_label l =
     if not (List.mem l !used) then
       used := l :: !used
   in
   List.iter (fun instr ->
     match instr with
     | Goto l -> add_label l
     | IfGoto (_, l) -> add_label l
     | IfNotGoto (_, l) -> add_label l
     | _ -> ()
   ) instrs;
   !used
 
 (* 标记可达的基本块 - 从 entry 开始遍历 *)
 let mark_reachable_blocks instrs =
   let reachable = ref [] in
   
   let rec visit label =
     if not (List.mem label !reachable) then (
       reachable := label :: !reachable;
       (* 找到该标签后的指令，收集跳转目标 *)
       let rec scan = function
         | [] -> ()
         | Label _ :: rest -> 
             (* 遇到新标签停止扫描（基本块结束） *)
             scan rest
         | Goto l :: _ -> 
             visit l  (* 无条件跳转 *)
         | IfGoto (_, l) :: rest -> 
             visit l;  (* 条件跳转的一个目标 *)
             scan rest  (* 继续扫描后续指令（fall-through） *)
         | IfNotGoto (_, l) :: rest -> 
             visit l;  (* 条件跳转的一个目标 *)
             scan rest  (* 继续扫描后续指令（fall-through） *)
         | _ :: rest -> 
             scan rest
       in
       (* 查找该标签后的指令序列 *)
       let rec find_block = function
         | [] -> []
         | Label l' :: rest when l' = label -> rest
         | _ :: rest -> find_block rest
       in
       scan (find_block instrs)
     )
   in
   (* 从 entry 标签开始遍历 *)
   visit "entry";
   
   (* 如果某个标签没有被标记但被使用了，也标记它（保守策略） *)
   let used_labels = collect_used_labels instrs in
   List.iter (fun l ->
     if List.mem l used_labels && not (List.mem l !reachable) then
       reachable := l :: !reachable
   ) used_labels;
   
   !reachable
 
 (* 查找指令序列中最后一个 Return 的位置 *)
 let find_last_return instrs =
   let rec find idx = function
     | [] -> -1
     | Return _ :: rest -> 
         let next = find (idx + 1) rest in
         if next = -1 then idx else next
     | _ :: rest -> find (idx + 1) rest
   in
   find 0 instrs
 
 (* 删除 return 后的不可达代码 *)
 let remove_after_return instrs =
   let last_return_idx = find_last_return instrs in
   if last_return_idx = -1 then
     instrs
   else
     let rec filter_idx idx = function
       | [] -> []
       | hd :: tl when idx <= last_return_idx -> hd :: filter_idx (idx + 1) tl
       | _ :: tl -> filter_idx (idx + 1) tl
     in
     filter_idx 0 instrs
 
 (* 删除空标签块 *)
let remove_empty_blocks instrs reachable =
    let rec clean = function
      | [] -> []
      | Label l :: rest ->
          if List.mem l reachable then
            (* 检查该标签后的内容（直到下一个标签或结束） *)
            let rec block_content = function
              | [] -> []
              | Label _ :: _ -> []
              | instr :: rest' -> instr :: block_content rest'
            in
            let content = block_content rest in
            if content = [] then
              (* 空标签块，删除标签和后续内容（如果有） *)
              let rec skip_to_next_label = function
                | [] -> []
                | (Label _) :: rest' -> rest'
                | _ :: rest' -> skip_to_next_label rest'
              in
              clean (skip_to_next_label rest)
            else
              Label l :: clean rest
          else
            (* 不可达标签，删除 *)
            let rec skip_to_next_label = function
              | [] -> []
              | (Label _) :: rest' -> rest'
              | _ :: rest' -> skip_to_next_label rest'
            in
            clean (skip_to_next_label rest)
      | instr :: rest -> instr :: clean rest
    in
    clean instrs
 
 (* 主要死代码删除函数 - 增强版 *)
 let dead_code_elimination (instrs: tac list) : tac list =
   if instrs = [] then []
   else
     (* 第一步：删除 return 后的代码 *)
     let after_removal = remove_after_return instrs in
     
     (* 第二步：标记可达的基本块 *)
     let reachable = mark_reachable_blocks after_removal in
     
     (* 第三步：收集所有被使用的变量 *)
     let used_vars = collect_used_vars after_removal in
     
     (* 第四步：过滤指令 *)
     let filtered = List.filter (fun instr ->
       match instr with
       | Label l ->
           (* 只保留可达的标签 *)
           List.mem l reachable
       | Goto _ | IfGoto (_, _) | IfNotGoto (_, _) ->
           (* 保留跳转指令，即使目标不可达也要保留（保守策略） *)
           has_side_effect instr
       | _ ->
           match defined_var instr with
           | Some var ->
               (* 如果定义的变量被使用，或者指令有副作用，则保留 *)
               if List.mem var used_vars then
                 true
               else
                 has_side_effect instr
           | None ->
               has_side_effect instr
     ) after_removal in
     
     (* 第五步：删除空标签块和不可达标签块 *)
     let cleaned = remove_empty_blocks filtered reachable in
     
     cleaned
 
 (* 对基本块进行死代码删除 *)
 let dead_code_elimination_block (block: basic_block) : basic_block =
   { block with instrs = dead_code_elimination block.instrs }
 
 (* 清理局部变量列表 - 只保留被使用的变量 *)
 let clean_locals (f: ir_func) (instrs: tac list) =
   let used_vars = collect_used_vars instrs in
   let new_locals = List.filter (fun v -> List.mem v used_vars) f.locals in
   { f with locals = new_locals }
 
 (* 对整个IR函数进行完整优化（常量折叠 + 死代码删除） *)
let optimize_function (f: ir_func) : ir_func =
    Printf.eprintf "Optimizing function: %s\n" f.fname;
    
    let count_instrs (func: ir_func) =
      List.length func.entry.instrs + 
      List.fold_left (fun acc (b: basic_block) -> acc + List.length b.instrs) 0 func.blocks
    in
    
    let before_count = count_instrs f in
    
    (* 第一步：常量折叠 *)
    let entry_folded = fold_constants_block f.entry in
    let blocks_folded = List.map fold_constants_block f.blocks in
    let f_folded = { f with entry = entry_folded; blocks = blocks_folded } in
    
    (* 第二步：死代码删除 *)
    let entry_dce = dead_code_elimination_block f_folded.entry in
    let blocks_dce = List.map dead_code_elimination_block f_folded.blocks in
    let f_dce = { f_folded with entry = entry_dce; blocks = blocks_dce } in
    
    (* 第三步：清理未使用的局部变量 *)
    let all_instrs = 
      let instrs1 = entry_dce.instrs in
      let instrs2 = List.fold_left (fun acc (b: basic_block) -> acc @ b.instrs) [] blocks_dce in
      instrs1 @ instrs2
    in
    let f_cleaned = clean_locals f_dce all_instrs in
    
    let after_count = count_instrs f_cleaned in
    
    if before_count > after_count then
      Printf.eprintf "  Removed %d instructions (%d -> %d, reduced %.1f%%)\n" 
        (before_count - after_count) before_count after_count
        (float_of_int (before_count - after_count) *. 100.0 /. float_of_int before_count);
    
    f_cleaned
 
 (* 对IR程序进行完整优化 *)
 let optimize_program (prog: ir_program) : ir_program =
   Printf.eprintf "=== Starting full optimization (constant folding + dead code elimination) ===\n";
   
   let count_total (prog: ir_program) =
     List.fold_left (fun acc item ->
       match item with
       | Function f ->
           acc + List.length f.entry.instrs + 
           List.fold_left (fun sum (b: basic_block) -> sum + List.length b.instrs) 0 f.blocks
       | GlobalVar _ -> acc
     ) 0 prog
   in
   
   let total_before = count_total prog in
   Printf.eprintf "Total instructions before optimization: %d\n" total_before;
   
   let optimized = List.map (function
     | Function f -> Function (optimize_function f)
     | GlobalVar _ as g -> g
   ) prog in
   
   let total_after = count_total optimized in
   Printf.eprintf "Total instructions after optimization: %d\n" total_after;
   
   if total_before > total_after then
     Printf.eprintf "Total reduction: %d instructions (%.1f%%)\n" 
       (total_before - total_after)
       (float_of_int (total_before - total_after) *. 100.0 /. float_of_int total_before);
   
   optimized
 
 (* 保持向后兼容 *)
 let optimize_constants (prog: ir_program) : ir_program =
   optimize_program prog
 
 
 (* ================================================================
    打印函数
    ================================================================ *)
 
 (* 操作数转字符串用于打印 *)
 let op_str = function
   | Const n -> string_of_int n
   | Var s -> s
   | Temp n -> "t" ^ string_of_int n
 
 (* AST二元运算符转字符串 *)
 let binop_str = Ast.(function
   | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
   | Eq -> "==" | Ne -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
   | And -> "&&" | Or -> "||"
 )
 
 (* 将 AST 一元运算符转换为对应的字符串表示 *)
 let unop_str = Ast.(function
   | Pos -> "+" | Neg -> "-" | Not -> "!"
 )
 
 (* 将单条 TAC 指令转换为可读的字符串 *)
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
   | Empty -> ""
 
 (* 打印一个基本块的标签及其所有指令 *)
 let dump_block b =
   Printf.eprintf "%s:\n" b.label;
   List.iter (fun i -> Printf.eprintf "  %s\n" (tac_str i)) b.instrs
 
 (* 打印一个 IR 函数的完整信息（参数、局部变量、基本块） *)
 let dump_func f =
   Printf.eprintf "\nfunc %s(%s):\n" f.fname (String.concat ", " f.params);
   Printf.eprintf "  locals: [%s]\n" (String.concat ", " f.locals);
   Printf.eprintf "  temps: %d\n\n" f.temps;
   dump_block f.entry;
   List.iter dump_block f.blocks
 
 (* 打印整个 IR 程序（全局变量和所有函数） *)
 let dump_ir prog =
   List.iter (function
     | GlobalVar (name, Some v) -> Printf.eprintf "global %s = %d\n" name v
     | GlobalVar (name, None) -> Printf.eprintf "global %s\n" name
     | Function f -> dump_func f
   ) prog