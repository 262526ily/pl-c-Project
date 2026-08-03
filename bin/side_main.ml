%{
open Ast
%}

let word_size = 4
let stack_align = 16

let reg_a0 = "a0"
let reg_sp = "sp"
let reg_fp = "fp"
let reg_ra = "ra"
let reg_t0 = "t0"

type env = (string * int) list

let lookup_var env var =
  try List.assoc var env
  with Not_found -> failwith ("Variable not found: " ^ var)

let add_var env var offset =
  (var, offset) :: env

let rec count_local_vars stmt =
    match stmt with
    | SBlock stmts -> List.fold_left (fun acc s -> acc + count_local_vars s) 0 stmts
    | SDecl (VarDecl (_, _)) -> 1
    | SDecl (ConstDecl _) -> 1
    | _ -> 0

let rec compile_expr (env : env) (e : expr) : string =
    match e with
    | EInt n -> Printf.sprintf "li %s, %d\n" reg_a0 n
    | EId x -> Printf.sprintf "lw %s, %d(%s)\n" reg_a0 (lookup_var env x) reg_fp
    | EBinop (op, e1, e2) -> failwith "TODO: Binop"
    | ECall (fname, args) -> failwith "TODO: Function call"
    | EUnop _ -> failwith "TODO: Unop"

and compile_stmt (env : env) (s : stmt) : string =
    match s with
    | SBlocks stmts -> List.map (compile_stmt env) stmts |> String.concat ""
    | SEmpty -> ""
    | SDecl (VarDecl (x, e)) -> failwith "TODO: Variable declaration"
    | SDecl (ConstDecl (x, e)) -> failwith "TODO: Constant declaration"
    | SAssign (x, e) -> failwith "TODO: Assignment"
    | SIf (cond, then_stmt, else_stmt) -> failwith "TODO: If statement"
    | SWhile (cond, body) -> failwith "TODO: While loop"
    | SReturn e_opt -> failwith "TODO: Return statement"
    | SBreak | SContinue -> failwith "TODO: Break/Continue statement"

let label_count = ref 0
let fresh_label prefix =
    incr label_count;
    Printf.sprintf "%s_%d" prefix !label_count

let emit fmt = Printf.sprintf (fmt ^^ "\n")