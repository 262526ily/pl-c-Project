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

(* ---------- register-direct codegen helpers ---------- *)

let operand_reg op reg_map =
  match op with
  | Const _ -> None
  | Var _ | Temp _ -> Hashtbl.find_opt reg_map op

let emit_assign x y map reg_map =
  match y with
  | Const n ->
      (match operand_reg x reg_map with
       | Some rx -> Printf.printf "    li %s, %d\n" rx n
       | None ->
           Printf.printf "    li t0, %d\n" n;
           store_op "t0" x map reg_map)
  | Var _ | Temp _ ->
      (match operand_reg x reg_map, operand_reg y reg_map with
       | Some rx, Some ry ->
           if rx <> ry then Printf.printf "    mv %s, %s\n" rx ry
       | Some rx, None ->
           load_op rx y map reg_map
       | None, Some ry ->
           store_op ry x map reg_map
       | None, None ->
           load_op "t0" y map reg_map;
           store_op "t0" x map reg_map)

let emit_unop x op y map reg_map =
  match op with
  | Ast.Pos -> emit_assign x y map reg_map
  | Ast.Neg | Ast.Not as unop ->
      let op_instr =
        match unop with
        | Ast.Neg -> "neg"
        | Ast.Not -> "seqz"
        | Ast.Pos -> assert false
      in
      (match operand_reg x reg_map, y with
       | Some rx, (Var _ | Temp _) ->
           (match operand_reg y reg_map with
            | Some ry -> Printf.printf "    %s %s, %s\n" op_instr rx ry
            | None ->
                load_op rx y map reg_map;
                Printf.printf "    %s %s, %s\n" op_instr rx rx)
       | None, (Var _ | Temp _) ->
           (match operand_reg y reg_map with
            | Some ry ->
                Printf.printf "    %s t0, %s\n" op_instr ry;
                store_op "t0" x map reg_map
            | None ->
                load_op "t0" y map reg_map;
                Printf.printf "    %s t0, t0\n" op_instr;
                store_op "t0" x map reg_map)
       | Some rx, Const n ->
           Printf.printf "    li t0, %d\n" n;
           Printf.printf "    %s %s, t0\n" op_instr rx
       | None, Const n ->
           Printf.printf "    li t0, %d\n" n;
           Printf.printf "    %s t0, t0\n" op_instr;
           store_op "t0" x map reg_map)

let emit_reg_binop dest op y z map reg_map =
  let src_reg op fallback =
    match operand_reg op reg_map with
    | Some r -> r
    | None -> load_op fallback op map reg_map; fallback
  in
  let ry = src_reg y "t0" in
  let rz = src_reg z "t1" in
  match op with
  | Ast.Add -> Printf.printf "    add %s, %s, %s\n" dest ry rz
  | Ast.Sub -> Printf.printf "    sub %s, %s, %s\n" dest ry rz
  | Ast.Mul -> Printf.printf "    mul %s, %s, %s\n" dest ry rz
  | Ast.Div -> Printf.printf "    div %s, %s, %s\n" dest ry rz
  | Ast.Mod -> Printf.printf "    rem %s, %s, %s\n" dest ry rz
  | Ast.And -> Printf.printf "    and %s, %s, %s\n" dest ry rz
  | Ast.Or -> Printf.printf "    or %s, %s, %s\n" dest ry rz
  | Ast.Eq ->
      Printf.printf "    sub %s, %s, %s\n" dest ry rz;
      Printf.printf "    sltiu %s, %s, 1\n" dest dest
  | Ast.Ne ->
      Printf.printf "    sub %s, %s, %s\n" dest ry rz;
      Printf.printf "    sltu %s, zero, %s\n" dest dest
  | Ast.Lt -> Printf.printf "    slt %s, %s, %s\n" dest ry rz
  | Ast.Gt -> Printf.printf "    slt %s, %s, %s\n" dest rz ry
  | Ast.Le ->
      Printf.printf "    slt %s, %s, %s\n" dest rz ry;
      Printf.printf "    xori %s, %s, 1\n" dest dest
  | Ast.Ge ->
      Printf.printf "    slt %s, %s, %s\n" dest ry rz;
      Printf.printf "    xori %s, %s, 1\n" dest dest

let dest_reg x reg_map =
  match operand_reg x reg_map with Some rx -> rx | None -> "t0"

let emit_add_imm x y imm map reg_map =
  let dest = dest_reg x reg_map in
  (match operand_reg y reg_map with
   | Some ry -> Printf.printf "    addi %s, %s, %d\n" dest ry imm
   | None ->
       load_op dest y map reg_map;
       Printf.printf "    addi %s, %s, %d\n" dest dest imm);
  if operand_reg x reg_map = None then store_op dest x map reg_map

let emit_binop x op y z map reg_map =
  match op with
  | Ast.Add
    when is_const_op y && not (is_const_op z)
         && is_imm12 (get_const_val y) ->
      emit_add_imm x z (get_const_val y) map reg_map
  | Ast.Add
    when is_const_op z && not (is_const_op y)
         && is_imm12 (get_const_val z) ->
      emit_add_imm x y (get_const_val z) map reg_map
  | Ast.Sub
    when is_const_op z && not (is_const_op y)
         && is_imm12 (- (get_const_val z)) ->
      emit_add_imm x y (- (get_const_val z)) map reg_map
  | Ast.Mul | Ast.Div | Ast.Mod
    when not (is_const_op y) && not (is_const_op z) ->
      let dest = dest_reg x reg_map in
      emit_reg_binop dest op y z map reg_map;
      if operand_reg x reg_map = None then store_op dest x map reg_map
  | Ast.Mul -> emit_mul x y z map reg_map
  | Ast.Div -> emit_div x y z map reg_map
  | Ast.Mod -> emit_mod x y z map reg_map
  | _ ->
      let dest = dest_reg x reg_map in
      emit_reg_binop dest op y z map reg_map;
      if operand_reg x reg_map = None then store_op dest x map reg_map

let emit_tac fname tac_inst map reg_map current_args =
  match tac_inst with
  | Assign (x, y) ->
      emit_assign x y map reg_map

  | AssignBinOp (x, op, y, z) ->
      emit_binop x op y z map reg_map

  | AssignUnOp (x, op, y) ->
      emit_unop x op y map reg_map

  | Goto l ->
      Printf.printf "    j %s\n" l

  | IfGoto (x, l) ->
      (match operand_reg x reg_map with
       | Some rx -> Printf.printf "    bnez %s, %s\n" rx l
       | None ->
           load_op "t0" x map reg_map;
           Printf.printf "    bnez t0, %s\n" l)

  | IfNotGoto (x, l) ->
      (match operand_reg x reg_map with
       | Some rx -> Printf.printf "    beqz %s, %s\n" rx l
       | None ->
           load_op "t0" x map reg_map;
           Printf.printf "    beqz t0, %s\n" l)

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

  (* Loop-aware frequency allocation: weight a use by 10^(estimated loop depth).
     Deeper loops dominate the register budget, which keeps hot induction and
     accumulation variables in callee-saved registers without ever sharing a
     physical register between two logical values. *)
  let blocks_order = f.entry :: f.blocks in
  let blocks_array = Array.of_list blocks_order in
  let label_to_index = Hashtbl.create 16 in
  Array.iteri (fun i b -> Hashtbl.replace label_to_index b.label i) blocks_array;
  let depths = Array.make (Array.length blocks_array) 0 in
  let collect_targets instrs =
    List.fold_left (fun acc -> function
      | Goto l -> l :: acc
      | IfGoto (_, l) | IfNotGoto (_, l) -> l :: acc
      | _ -> acc)
      [] instrs
  in
  let back_edges = ref [] in
  Array.iteri (fun i b ->
    List.iter (fun l ->
      match Hashtbl.find_opt label_to_index l with
      | Some j when j <= i -> back_edges := (j, i) :: !back_edges
      | _ -> ())
      (collect_targets b.instrs))
    blocks_array;
  let back_edges_sorted =
    List.sort
      (fun (a1, b1) (a2, b2) -> compare (b1 - a1) (b2 - a2))
      !back_edges
  in
  List.iter (fun (h, t) ->
    let m = ref 0 in
    for k = h to t do
      if depths.(k) > !m then m := depths.(k)
    done;
    for k = h to t do
      if depths.(k) < !m + 1 then depths.(k) <- !m + 1
    done)
    back_edges_sorted;
  let weight_for_depth d =
    let rec loop n acc = if n <= 0 then acc else loop (n - 1) (acc * 10) in
    loop d 1
  in

  let use_count = Hashtbl.create 64 in
  let bump op weight =
    let n = (match Hashtbl.find_opt use_count op with Some n -> n | None -> 0) in
    Hashtbl.replace use_count op (n + weight)
  in
  let bump_instr weight = function
    | Assign (x, y) -> bump x weight; bump y weight
    | AssignBinOp (x, _, y, z) -> bump x weight; bump y weight; bump z weight
    | AssignUnOp (x, _, y) -> bump x weight; bump y weight
    | IfGoto (x, _) | IfNotGoto (x, _) -> bump x weight
    | Param x -> bump x weight
    | Call (x, _, _) -> bump x weight
    | Return (Some x) -> bump x weight
    | Goto _ | Label _ | Return None -> ()
  in
  Array.iteri (fun i b ->
    let weight = weight_for_depth depths.(i) in
    List.iter (bump_instr weight) b.instrs)
    blocks_array;

  let rec range a b =
    if a > b then [] else a :: range (a + 1) b
  in
  let candidates =
    List.map (fun name -> Var name) f.params
    @ List.map (fun name -> Var name) f.locals
    @ List.map (fun t -> Temp t) (range 0 (f.temps - 1))
  in
  let compare_candidates a b =
    let ca = (match Hashtbl.find_opt use_count a with Some n -> n | None -> 0) in
    let cb = (match Hashtbl.find_opt use_count b with Some n -> n | None -> 0) in
    if ca <> cb then compare cb ca else compare a b
  in
  let sorted_candidates = List.sort compare_candidates candidates in

  let reg_map = Hashtbl.create 32 in
  let used_regs = ref [] in
  let rec assign_pool regs = function
    | [] -> ()
    | _ when regs = [] -> ()
    | op :: rest ->
        (match regs with
         | r :: rs ->
             Hashtbl.replace reg_map op r;
             used_regs := r :: !used_regs;
             assign_pool rs rest
         | [] -> ())
  in
  assign_pool reg_pool sorted_candidates;
  let used_regs_list = List.rev !used_regs in  let saved_count = List.length used_regs_list in
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
