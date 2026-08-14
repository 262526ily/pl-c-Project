(* lib/codegen.ml *)
open Ir

(* 唯一标签计数器 *)
let inline_label_counter = ref 0

let gen_inline_label prefix =
  incr inline_label_counter;
  Printf.sprintf "%s_inline_%d" prefix !inline_label_counter

(* 辅助函数：列表切分 *)
let rec split_at n = function
  | xs when n <= 0 -> [], xs
  | [] -> [], []
  | x :: xs ->
      let prefix, suffix = split_at (n - 1) xs in
      x :: prefix, suffix

(* ============================================================ *)
(* 常量判断和数学工具 *)

(* 判断操作数是否为常量 *)
let is_const_op = function Const _ -> true | _ -> false
let get_const_val = function Const n -> n | _ -> 0

(* 判断是否为 2 的幂 *)
let is_power_of_two n =
  n > 0 && (n land (n - 1)) = 0

let log2 n =
  let rec loop x acc =
    if x = 1 then acc
    else loop (x lsr 1) (acc + 1)
  in
  if n <= 0 then 0 else loop n 0

(* 检查立即数是否在 12 位范围内 *)
let is_imm12 n = n >= -2048 && n <= 2047

(* ============================================================ *)
(* 安全的偏移量查找 *)

let find_offset map op =
  match Hashtbl.find_opt map op with
  | Some off -> off
  | None ->
      match op with
      | Var name ->
          Printf.eprintf "Warning: Variable '%s' not found in offset map, treating as global\n" name;
          -1
      | Temp t ->
          Printf.eprintf "Fatal: Temp %d not found in offset map\n" t;
          exit 1
      | Const _ ->
          Printf.eprintf "Fatal: Const should not be in offset map\n";
          exit 1

(* ============================================================ *)
(* 加载和存储操作数 *)

let load_op reg op map reg_map =
  match op with
  | Const n ->
      Printf.printf "    li %s, %d\n" reg n
  | Temp t ->
      (match Hashtbl.find_opt reg_map (Temp t) with
       | Some phys ->
           Printf.printf "    mv %s, %s\n" reg phys
       | None ->
           if Hashtbl.mem map (Temp t) then
             let off = Hashtbl.find map (Temp t) in
             Printf.printf "    lw %s, %d(fp)\n" reg off
           else (
             Printf.eprintf "ERROR: Temp %d not found in offset map\n" t;
             exit 1
           ))
  | Var name ->
      (match Hashtbl.find_opt reg_map (Var name) with
       | Some phys ->
           Printf.printf "    mv %s, %s\n" reg phys
       | None ->
           if Hashtbl.mem map (Var name) then
             let off = Hashtbl.find map (Var name) in
             Printf.printf "    lw %s, %d(fp)\n" reg off
           else
             (Printf.printf "    la %s, %s\n" reg name;
              Printf.printf "    lw %s, 0(%s)\n" reg reg))

let store_op reg op map reg_map =
  match op with
  | Const _ -> ()
  | Temp t ->
      (match Hashtbl.find_opt reg_map (Temp t) with
       | Some phys ->
           Printf.printf "    mv %s, %s\n" phys reg
       | None ->
           if Hashtbl.mem map (Temp t) then
             let off = Hashtbl.find map (Temp t) in
             Printf.printf "    sw %s, %d(fp)\n" reg off
           else (
             Printf.eprintf "ERROR: Temp %d not found in offset map for store\n" t;
             exit 1
           ))
  | Var name ->
      (match Hashtbl.find_opt reg_map (Var name) with
       | Some phys ->
           Printf.printf "    mv %s, %s\n" phys reg
       | None ->
           if Hashtbl.mem map (Var name) then
             let off = Hashtbl.find map (Var name) in
             Printf.printf "    sw %s, %d(fp)\n" reg off
           else
             (Printf.printf "    la t3, %s\n" name;
              Printf.printf "    sw %s, 0(t3)\n" reg))

(* ============================================================ *)
(* ?????????????????? *)

let allocate_registers (f: ir_func) reg_pool =
  let blocks_order = f.entry :: f.blocks in

  let linear_instrs =
    let acc = ref [] in
    let add_block b =
      let rec loop = function
        | [] -> ()
        | ((Goto _ | Return _) as i) :: _ ->
            acc := i :: !acc
        | i :: rest ->
            acc := i :: !acc;
            loop rest
      in
      loop b.instrs
    in
    List.iter add_block blocks_order;
    Array.of_list (List.rev !acc)
  in

  let n = Array.length linear_instrs in
  let label_map = Hashtbl.create 32 in
  Array.iteri (fun i instr ->
      match instr with
      | Label l -> Hashtbl.replace label_map l i
      | _ -> ())
    linear_instrs;

  let succ = Array.make n [] in
  for i = 0 to n - 1 do
    match linear_instrs.(i) with
    | Goto l ->
        succ.(i) <- (match Hashtbl.find_opt label_map l with Some j -> [j] | None -> [])
    | IfGoto (_, l) | IfNotGoto (_, l) ->
        let target = (match Hashtbl.find_opt label_map l with Some j -> [j] | None -> []) in
        let fall = if i + 1 < n then [i + 1] else [] in
        succ.(i) <- target @ fall
    | Return _ ->
        succ.(i) <- []
    | _ ->
        succ.(i) <- (if i + 1 < n then [i + 1] else [])
  done;

  let local_set = Hashtbl.create 32 in
  List.iter (fun name -> Hashtbl.replace local_set (Var name) ()) f.params;
  List.iter (fun name -> Hashtbl.replace local_set (Var name) ()) f.locals;
  let is_allocatable = function
    | Var name -> Hashtbl.mem local_set (Var name)
    | Temp _ -> true
    | Const _ -> false
  in

  let use_arr = Array.make n [] in
  let def_arr = Array.make n [] in
  let add_use i op = if is_allocatable op then use_arr.(i) <- op :: use_arr.(i) in
  let add_def i op = if is_allocatable op then def_arr.(i) <- op :: def_arr.(i) in
  Array.iteri (fun i instr ->
      match instr with
      | Assign (x, y) -> add_def i x; add_use i y
      | AssignBinOp (x, _, y, z) -> add_def i x; add_use i y; add_use i z
      | AssignUnOp (x, _, y) -> add_def i x; add_use i y
      | IfGoto (x, _) | IfNotGoto (x, _) -> add_use i x
      | Param x -> add_use i x
      | Call (x, _, _) -> add_def i x
      | Return (Some x) -> add_use i x
      | Goto _ | Label _ | Return None -> ())
    linear_instrs;

  let rec union acc = function
    | [] -> acc
    | x :: rest ->
        if List.mem x acc then union acc rest else union (x :: acc) rest
  in
  let rec diff a = function
    | [] -> a
    | x :: rest -> diff (List.filter (fun y -> y <> x) a) rest
  in

  let live_in = Array.make n [] in
  let live_out = Array.make n [] in
  for i = n - 1 downto 0 do
    live_out.(i) <- List.fold_left (fun acc j -> union acc live_in.(j)) [] succ.(i);
    live_in.(i) <- union use_arr.(i) (diff live_out.(i) def_arr.(i))
  done;

  let intervals = Hashtbl.create 64 in
  let update op i =
    let st = i and stop = i + 1 in
    match Hashtbl.find_opt intervals op with
    | None -> Hashtbl.replace intervals op (st, stop)
    | Some (a, b) -> Hashtbl.replace intervals op (min a st, max b stop)
  in
  for i = 0 to n - 1 do
    List.iter (fun op -> update op i) live_in.(i);
    List.iter (fun op -> update op i) live_out.(i)
  done;

  let interval_list =
    Hashtbl.fold (fun op (st, stop) acc -> (op, st, stop) :: acc) intervals []
  in
  let interval_list =
    List.sort (fun (_, s1, _) (_, s2, _) -> compare s1 s2) interval_list
  in

  let reg_map = Hashtbl.create 32 in
  let free_regs = ref reg_pool in
  let active = ref [] in
  let expire st =
    let expired, remaining = List.partition (fun (_, _, stop) -> stop <= st) !active in
    List.iter (fun (_, r, _) -> free_regs := r :: !free_regs) expired;
    active := remaining
  in
  List.iter (fun (op, st, stop) ->
      expire st;
      match !free_regs with
      | r :: rest ->
          free_regs := rest;
          Hashtbl.replace reg_map op r;
          active := (op, r, stop) :: !active
      | [] -> ())
    interval_list;

  let used_regs_list =
    List.filter
      (fun r ->
        Hashtbl.fold (fun _ r' acc -> acc || r = r') reg_map false)
      reg_pool
  in
  (reg_map, used_regs_list)

(* ============================================================ *)
(* ?????????? *)

let compute_offsets (f: ir_func) =
  let local_slots = ref 0 in
  let map = Hashtbl.create 32 in

  List.iteri (fun i name ->
    if i < 8 then (
      incr local_slots;
      Hashtbl.add map (Var name) (-8 - 4 * !local_slots)
    ) else (
      Hashtbl.add map (Var name) ((i - 8) * 4)
    )
  ) f.params;

  List.iter (fun name ->
    if not (Hashtbl.mem map (Var name)) then (
      incr local_slots;
      Hashtbl.add map (Var name) (-8 - 4 * !local_slots)
    )
  ) f.locals;

  for t = 0 to f.temps - 1 do
    incr local_slots;
    Hashtbl.add map (Temp t) (-8 - 4 * !local_slots)
  done;

  (!local_slots, map)

(* ============================================================ *)
(* ?????????? *)

let emit_mul x y z map reg_map =
  let is_y_const = is_const_op y in
  let is_z_const = is_const_op z in

  if is_z_const then
    let n = get_const_val z in
    load_op "t0" y map reg_map;
    if n = 0 then
      Printf.printf "    li t0, 0\n"
    else if n = 1 then
      ()
    else if n = -1 then
      Printf.printf "    neg t0, t0\n"
    else if is_power_of_two n then
      let shift = log2 n in
      Printf.printf "    slli t0, t0, %d\n" shift
    else if n = 3 then
      (Printf.printf "    slli t1, t0, 1\n";
       Printf.printf "    add t0, t0, t1\n")
    else if n = 5 then
      (Printf.printf "    slli t1, t0, 2\n";
       Printf.printf "    add t0, t0, t1\n")
    else if n = 7 then
      (Printf.printf "    slli t1, t0, 3\n";
       Printf.printf "    sub t0, t1, t0\n")
    else if n = 9 then
      (Printf.printf "    slli t1, t0, 3\n";
       Printf.printf "    add t0, t0, t1\n")
    else if n = 10 then
      (Printf.printf "    slli t1, t0, 3\n";
       Printf.printf "    slli t2, t0, 1\n";
       Printf.printf "    add t0, t1, t2\n")
    else
      (load_op "t1" z map reg_map;
       Printf.printf "    mul t0, t0, t1\n")
  else if is_y_const then
    let n = get_const_val y in
    load_op "t0" z map reg_map;
    if n = 0 then
      Printf.printf "    li t0, 0\n"
    else if n = 1 then
      ()
    else if n = -1 then
      Printf.printf "    neg t0, t0\n"
    else if is_power_of_two n then
      let shift = log2 n in
      Printf.printf "    slli t0, t0, %d\n" shift
    else
      (load_op "t1" y map reg_map;
       Printf.printf "    mul t0, t0, t1\n")
  else
    (load_op "t0" y map reg_map;
     load_op "t1" z map reg_map;
     Printf.printf "    mul t0, t0, t1\n");
  store_op "t0" x map reg_map

let emit_div x y z map reg_map =
  if is_const_op z then
    let n = get_const_val z in
    load_op "t0" y map reg_map;
    if n = 1 then
      ()
    else if n = -1 then
      Printf.printf "    neg t0, t0\n"
    else if is_power_of_two n then
      let shift = log2 n in
      if shift >= 31 then
        (load_op "t1" z map reg_map;
         Printf.printf "    div t0, t0, t1\n")
      else (
        Printf.printf "    srai t1, t0, 31\n";
        Printf.printf "    srli t1, t1, %d\n" (32 - shift);
        Printf.printf "    add t0, t0, t1\n";
        Printf.printf "    srai t0, t0, %d\n" shift
      )
    else
      (load_op "t1" z map reg_map;
       Printf.printf "    div t0, t0, t1\n")
  else
    (load_op "t0" y map reg_map;
     load_op "t1" z map reg_map;
     Printf.printf "    div t0, t0, t1\n");
  store_op "t0" x map reg_map

let emit_mod x y z map reg_map =
  if is_const_op z then
    let n = get_const_val z in
    load_op "t0" y map reg_map;
    if n = 1 then
      Printf.printf "    li t0, 0\n"
    else if is_power_of_two n then
      let shift = log2 n in
      if shift >= 31 then
        (load_op "t1" z map reg_map;
         Printf.printf "    rem t0, t0, t1\n")
      else (
        let mask = n - 1 in
        Printf.printf "    srai t1, t0, 31\n";
        Printf.printf "    srli t1, t1, %d\n" (32 - shift);
        Printf.printf "    add t0, t0, t1\n";
        if is_imm12 mask then
          Printf.printf "    andi t0, t0, %d\n" mask
        else (
          Printf.printf "    li t2, %d\n" mask;
          Printf.printf "    and t0, t0, t2\n");
        Printf.printf "    sub t0, t0, t1\n"
      )
    else
      (load_op "t1" z map reg_map;
       Printf.printf "    rem t0, t0, t1\n")
  else
    (load_op "t0" y map reg_map;
     load_op "t1" z map reg_map;
     Printf.printf "    rem t0, t0, t1\n");
  store_op "t0" x map reg_map

let emit_tac fname tac_inst map reg_map current_args =
  match tac_inst with
  | Assign (x, y) ->
      load_op "t0" y map reg_map;
      store_op "t0" x map reg_map

  | AssignBinOp (x, op, y, z) ->
      (match op with
       | Ast.Mul -> emit_mul x y z map reg_map
       | Ast.Div -> emit_div x y z map reg_map
       | Ast.Mod -> emit_mod x y z map reg_map
       | _ ->
           load_op "t0" y map reg_map;
           load_op "t1" z map reg_map;
           (match op with
            | Ast.Add -> Printf.printf "    add t0, t0, t1\n"
            | Ast.Sub -> Printf.printf "    sub t0, t0, t1\n"
            | Ast.Eq  -> Printf.printf "    sub t0, t0, t1\n    sltiu t0, t0, 1\n"
            | Ast.Ne  -> Printf.printf "    sub t0, t0, t1\n    sltu t0, zero, t0\n"
            | Ast.Lt  -> Printf.printf "    slt t0, t0, t1\n"
            | Ast.Gt  -> Printf.printf "    slt t0, t1, t0\n"
            | Ast.Le  -> Printf.printf "    slt t0, t1, t0\n    xori t0, t0, 1\n"
            | Ast.Ge  -> Printf.printf "    slt t0, t0, t1\n    xori t0, t0, 1\n"
            | Ast.And -> Printf.printf "    and t0, t0, t1\n"
            | Ast.Or  -> Printf.printf "    or t0, t0, t1\n"
            | Ast.Mul | Ast.Div | Ast.Mod -> assert false
           );
           store_op "t0" x map reg_map)

  | AssignUnOp (x, op, y) ->
      load_op "t0" y map reg_map;
      (match op with
       | Ast.Pos -> ()
       | Ast.Neg -> Printf.printf "    neg t0, t0\n"
       | Ast.Not -> Printf.printf "    seqz t0, t0\n");
      store_op "t0" x map reg_map

  | Goto l ->
      Printf.printf "    j %s\n" l

  | IfGoto (x, l) ->
      load_op "t0" x map reg_map;
      Printf.printf "    bnez t0, %s\n" l

  | IfNotGoto (x, l) ->
      load_op "t0" x map reg_map;
      Printf.printf "    beqz t0, %s\n" l

  | Label l ->
      Printf.printf "%s:\n" l

  | Param x ->
      current_args := x :: !current_args

  | Call (dest, callee, nargs) ->
      let call_args, rem = split_at nargs !current_args in
      current_args := rem;
      let args = call_args in

      let extra_space = if nargs > 8 then ((nargs - 8) * 4 + 15) / 16 * 16 else 0 in
      if extra_space > 0 then
        Printf.printf "    addi sp, sp, -%d\n" extra_space;

      List.iteri (fun j arg ->
        if j < 8 then
          load_op (Printf.sprintf "a%d" j) arg map reg_map
        else
          (load_op "t0" arg map reg_map;
           Printf.printf "    sw t0, %d(sp)\n" ((j - 8) * 4))
      ) args;

      Printf.printf "    call %s\n" callee;

      if extra_space > 0 then
        Printf.printf "    addi sp, sp, %d\n" extra_space;

      store_op "a0" dest map reg_map

  | Return (Some x) ->
      load_op "a0" x map reg_map;
      Printf.printf "    j .L_epilogue_%s\n" fname

  | Return None ->
      Printf.printf "    j .L_epilogue_%s\n" fname

let emit_block fname (b: basic_block) map reg_map current_args =
  if b.label <> "entry" then
   Printf.printf "%s:\n" b.label;
  let rec emit_until_terminator = function
    | [] -> ()
    | inst :: rest ->
        emit_tac fname inst map reg_map current_args;
        match inst with
        | Return _ | Goto _ ->
            ()
        | _ ->
            emit_until_terminator rest
  in
  emit_until_terminator b.instrs

let emit_function (f: ir_func) =
  let slots, map = compute_offsets f in

  let reg_pool = ["s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"] in
  let reg_map, used_regs_list = allocate_registers f reg_pool in
  let saved_count = List.length used_regs_list in
  let framesize = ((8 + slots * 4 + saved_count * 4 + 15) / 16) * 16 in

  Printf.printf "    .globl %s\n" f.fname;
  Printf.printf "%s:\n" f.fname;

  Printf.printf "    addi sp, sp, -%d\n" framesize;
  Printf.printf "    sw ra, %d(sp)\n" (framesize - 4);
  Printf.printf "    sw fp, %d(sp)\n" (framesize - 8);
  Printf.printf "    addi fp, sp, %d\n" framesize;

  List.iteri (fun i r ->
      let off = -8 - 4 * (slots + i + 1) in
      Printf.printf "    sw %s, %d(fp)\n" r off)
    used_regs_list;

  List.iteri (fun i name ->
      let op = Var name in
      match Hashtbl.find_opt reg_map op with
      | Some r ->
          if i < 8 then
            Printf.printf "    mv %s, a%d\n" r i
          else
            Printf.printf "    lw %s, %d(fp)\n" r ((i - 8) * 4)
      | None ->
          if i < 8 then
            let off = Hashtbl.find map (Var name) in
            Printf.printf "    sw a%d, %d(fp)\n" i off)
    f.params;

  let current_args = ref [] in
  emit_block f.fname f.entry map reg_map current_args;
  List.iter (fun b -> emit_block f.fname b map reg_map current_args) f.blocks;

  Printf.printf ".L_epilogue_%s:\n" f.fname;
  List.iteri (fun i r ->
      let off = -8 - 4 * (slots + i + 1) in
      Printf.printf "    lw %s, %d(fp)\n" r off)
    used_regs_list;
  Printf.printf "    lw ra, -4(fp)\n";
  Printf.printf "    lw fp, -8(fp)\n";
  Printf.printf "    addi sp, sp, %d\n" framesize;
  Printf.printf "    ret\n\n"

let generate_riscv (prog: ir_program) =
  Printf.printf "    .text\n\n";

  List.iter (function
    | GlobalVar (name, Some v, _) ->
        Printf.printf "    .globl %s\n" name;
        Printf.printf "    .data\n";
        Printf.printf "    .align 2\n";
        Printf.printf "%s:\n" name;
        Printf.printf "    .word %d\n\n" v
    | GlobalVar (name, None, _) ->
        Printf.printf "    .globl %s\n" name;
        Printf.printf "    .data\n";
        Printf.printf "    .align 2\n";
        Printf.printf "%s:\n" name;
        Printf.printf "    .space 4\n\n"
    | Function f ->
        Printf.printf "    .text\n";
        emit_function f
  ) prog