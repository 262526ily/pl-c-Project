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

let emit_imm_cmp x y imm invert map reg_map =
  let dest = dest_reg x reg_map in
  (match operand_reg y reg_map with
   | Some ry -> Printf.printf "    slti %s, %s, %d\n" dest ry imm
   | None ->
       load_op dest y map reg_map;
       Printf.printf "    slti %s, %s, %d\n" dest dest imm);
  if invert then Printf.printf "    xori %s, %s, 1\n" dest dest;
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
  | Ast.Lt
    when is_const_op z && not (is_const_op y)
         && is_imm12 (get_const_val z) ->
      emit_imm_cmp x y (get_const_val z) false map reg_map
  | Ast.Ge
    when is_const_op z && not (is_const_op y)
         && is_imm12 (get_const_val z) ->
      emit_imm_cmp x y (get_const_val z) true map reg_map
  | Ast.Le
    when is_const_op z && not (is_const_op y)
         && is_imm12 ((get_const_val z) + 1) ->
      emit_imm_cmp x y ((get_const_val z) + 1) false map reg_map
  | Ast.Gt
    when is_const_op z && not (is_const_op y)
         && is_imm12 ((get_const_val z) + 1) ->
      emit_imm_cmp x y ((get_const_val z) + 1) true map reg_map
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

  let has_calls =
    List.exists
      (fun (b: basic_block) ->
        List.exists (function Call _ -> true | _ -> false) b.instrs)
      (f.entry :: f.blocks)
  in
  let reg_pool =
    if has_calls then
      ["s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"]
    else
      ["s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11";
       "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "t4"; "t5"; "t6"]
  in

  (* Liveness-based linear-scan allocation: build CFG live-in/live-out sets,
     derive conservative live intervals, then assign physical registers by
     interval overlap and farthest-end spilling. *)
  let blocks_order : basic_block list = f.entry :: f.blocks in
  let blocks_array : basic_block array = Array.of_list blocks_order in
  let label_to_index : (string, int) Hashtbl.t = Hashtbl.create 16 in
  Array.iteri (fun i b ->
    Hashtbl.replace label_to_index b.label i;
    List.iter (function Label l -> Hashtbl.replace label_to_index l i | _ -> ()) b.instrs)
    blocks_array;
  let rec range a b =
    if a > b then [] else a :: range (a + 1) b
  in
  let candidates =
    List.map (fun name -> Var name) f.params
    @ List.map (fun name -> Var name) f.locals
    @ List.map (fun t -> Temp t) (range 0 (f.temps - 1))
  in
  let cand_array : operand array = Array.of_list candidates in
  let ncand : int = Array.length cand_array in
  let operand_index : (operand, int) Hashtbl.t = Hashtbl.create 64 in
  List.iteri (fun i op -> Hashtbl.replace operand_index op i) candidates;
  let idx_of op = Hashtbl.find_opt operand_index op in
  let reachable_instrs (b: basic_block) =
    let rec take = function
      | [] -> []
      | (Goto _ as i) :: _ -> [i]
      | (Return _ as i) :: _ -> [i]
      | i :: rest -> i :: take rest
    in
    take b.instrs
  in

  let defs_of_inst = function
    | Assign (x, _) -> [x]
    | AssignBinOp (x, _, _, _) -> [x]
    | AssignUnOp (x, _, _) -> [x]
    | Call (x, _, _) -> [x]
    | _ -> []
  in
  let uses_of_inst = function
    | Assign (_, y) -> [y]
    | AssignBinOp (_, _, y, z) -> [y; z]
    | AssignUnOp (_, _, y) -> [y]
    | IfGoto (x, _) | IfNotGoto (x, _) -> [x]
    | Param x -> [x]
    | Return (Some x) -> [x]
    | Goto _ | Label _ | Call _ | Return None -> []
  in

  let nb : int = Array.length blocks_array in
  let preds : int list array = Array.make nb [] in
  let collect_branch_targets instrs =
    List.fold_left (fun acc -> function
      | IfGoto (_, l) | IfNotGoto (_, l) -> l :: acc
      | _ -> acc)
      [] instrs
  in
  Array.iteri (fun i (b: basic_block) ->
      let tail_succ =
        match List.rev (reachable_instrs b) with
        | Goto l :: _ ->
            (match Hashtbl.find_opt label_to_index l with
             | Some j -> [j]
             | None -> [])
        | IfGoto (_, l) :: _ | IfNotGoto (_, l) :: _ ->
            let branch =
              match Hashtbl.find_opt label_to_index l with
              | Some j -> [j]
              | None -> []
            in
            (if i + 1 < nb then (i + 1) :: branch else branch)
        | Return _ :: _ -> []
        | _ ->
            (if i + 1 < nb then [i + 1] else [])
      in
      let branch_succ =
        List.filter_map
          (fun l -> Hashtbl.find_opt label_to_index l)
          (collect_branch_targets (reachable_instrs b))
      in
      let succ =
        List.fold_left
          (fun acc j -> if List.mem j acc then acc else acc @ [j])
          [] (tail_succ @ branch_succ)
      in
      List.iter (fun j -> preds.(j) <- i :: preds.(j)) succ)
    blocks_array;

  let block_def : bool array array = Array.init nb (fun _ -> Array.make ncand false) in
  let block_use : bool array array = Array.init nb (fun _ -> Array.make ncand false) in
  let mark_ops ops bits =
    List.iter (fun op ->
        match idx_of op with
        | Some k -> bits.(k) <- true
        | None -> ())
      ops
  in
  Array.iteri (fun i (b: basic_block) ->
      List.iter (fun inst ->
          mark_ops (defs_of_inst inst) block_def.(i);
          mark_ops (uses_of_inst inst) block_use.(i))
        (reachable_instrs b))
    blocks_array;

  let live_in : bool array array = Array.init nb (fun _ -> Array.make ncand false) in
  let live_out : bool array array = Array.init nb (fun _ -> Array.make ncand false) in
  let compute_live_in b =
    let out = live_out.(b) in
    let defs = block_def.(b) in
    let uses = block_use.(b) in
    let res = Array.copy out in
    for k = 0 to ncand - 1 do if defs.(k) then res.(k) <- false done;
    for k = 0 to ncand - 1 do if uses.(k) then res.(k) <- true done;
    res
  in
  let bits_equal a b =
    let rec go k =
      if k >= ncand then true
      else if a.(k) <> b.(k) then false
      else go (k + 1)
    in
    go 0
  in
  let union_into dst src =
    for k = 0 to ncand - 1 do if src.(k) then dst.(k) <- true done
  in
  let queue = ref [] in
  for i = 0 to nb - 1 do queue := i :: !queue done;
  while !queue <> [] do
    let b = List.hd !queue in
    queue := List.tl !queue;
    let new_live_in = compute_live_in b in
    if not (bits_equal new_live_in live_in.(b)) then begin
      live_in.(b) <- new_live_in;
      List.iter (fun p ->
          union_into live_out.(p) new_live_in;
          if not (List.mem p !queue) then queue := p :: !queue)
        preds.(b)
    end
  done;

  let instr_arrays : tac array array =
    Array.map (fun (b: basic_block) -> Array.of_list (reachable_instrs b)) blocks_array
  in
  let block_start : int array = Array.make nb 0 in
  let next_pos = ref 0 in
  for i = 0 to nb - 1 do
    block_start.(i) <- !next_pos;
    next_pos := !next_pos + Array.length instr_arrays.(i)
  done;

  let first : int array = Array.make ncand max_int in
  let last : int array = Array.make ncand (-1) in
  let record k p =
    if p < first.(k) then first.(k) <- p;
    if p > last.(k) then last.(k) <- p
  in
  for i = nb - 1 downto 0 do
    let arr = instr_arrays.(i) in
    let live_after = ref (Array.copy live_out.(i)) in
    for j = Array.length arr - 1 downto 0 do
      let p = block_start.(i) + j in
      let inst = arr.(j) in
      let defs = defs_of_inst inst in
      let uses = uses_of_inst inst in
      let cur = !live_after in
      List.iter (fun op ->
          match idx_of op with
          | Some k -> record k p
          | None -> ())
        uses;
      List.iter (fun op ->
          match idx_of op with
          | Some k -> if cur.(k) then record k p
          | None -> ())
        defs;
      let next = Array.copy cur in
      List.iter (fun op ->
          match idx_of op with
          | Some k -> next.(k) <- false
          | None -> ())
        defs;
      List.iter (fun op ->
          match idx_of op with
          | Some k -> next.(k) <- true
          | None -> ())
        uses;
      live_after := next
    done
  done;

  (* Loop-carried live values are live through the whole loop body in source
     order (the backedge jumps backwards), so extend their intervals across
     every backedge whose header they are live-in to.  This keeps register
     sharing sound even though the source order is not a topological order of
     the CFG. *)
  for h = 0 to nb - 1 do
    List.iter (fun p ->
        if h <= p then
          let loop_start = block_start.(h) in
          let loop_end = block_start.(p) + Array.length instr_arrays.(p) - 1 in
          for k = 0 to ncand - 1 do
            if live_in.(h).(k) then begin
              if loop_start < first.(k) then first.(k) <- loop_start;
              if loop_end > last.(k) then last.(k) <- loop_end
            end
          done)
      preds.(h)
  done;

  let intervals : (int * int * operand) list ref = ref [] in
  for k = 0 to ncand - 1 do
    if last.(k) >= 0 then begin
      let op = cand_array.(k) in
      let start =
        match op with
        | Var name when List.mem name f.params -> 0
        | _ -> if first.(k) = max_int then 0 else first.(k)
      in
      intervals := (start, last.(k), op) :: !intervals
    end
  done;
  let intervals_sorted =
    List.sort (fun (s1, _, _) (s2, _, _) -> compare s1 s2) !intervals
  in

  let reg_map : (operand, string) Hashtbl.t = Hashtbl.create 32 in
  let reg_used : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let active : (int * string * operand) list ref = ref [] in
  let find_free active =
    let rec go = function
      | [] -> None
      | r :: rest ->
          if List.exists (fun (_, ar, _) -> ar = r) active then go rest
          else Some r
    in
    go reg_pool
  in
  let expire s active =
    List.filter (fun (e, _, _) -> e >= s) active
  in
  let assign op r =
    Hashtbl.replace reg_map op r;
    Hashtbl.replace reg_used r ()
  in
  let spill_farthest active =
    let rec go best = function
      | [] -> best
      | ((ae, _, _) as item) :: rest ->
          let (be, _, _) = best in
          if ae > be then go item rest else go best rest
    in
    match active with
    | [] -> (0, "", Var "")
    | hd :: _ -> go hd active
  in
  List.iter (fun (s, e, op) ->
      let cur_active = expire s !active in
      match find_free cur_active with
      | Some r ->
          assign op r;
          active := (e, r, op) :: cur_active
      | None ->
          if cur_active <> [] then begin
            let (maxe, maxr, maxop) = spill_farthest cur_active in
            if maxe > e then begin
              Hashtbl.remove reg_map maxop;
              active :=
                (e, maxr, op)
                :: List.filter (fun (_, _, o) -> o <> maxop) cur_active;
              assign op maxr
            end
          end)
    intervals_sorted;

  let used_regs_list =
    List.filter (fun r -> Hashtbl.mem reg_used r) reg_pool
  in
  let saved_regs =
    List.filter (fun r -> String.length r > 0 && r.[0] = 's') used_regs_list
  in
  let saved_count = List.length saved_regs in
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
    saved_regs;

  List.iteri (fun i name ->
      if i < 8 then
        let off = Hashtbl.find map (Var name) in
        Printf.printf "    sw a%d, %d(fp)\n" i off)
    f.params;

  List.iteri (fun i name ->
      match Hashtbl.find_opt reg_map (Var name) with
      | Some r ->
          if i < 8 then
            let off = Hashtbl.find map (Var name) in
            Printf.printf "    lw %s, %d(fp)\n" r off
          else
            Printf.printf "    lw %s, %d(fp)\n" r ((i - 8) * 4)
      | None -> ())
    f.params;

  let current_args = ref [] in
  emit_block f.fname f.entry map reg_map current_args;
  List.iter (fun b -> emit_block f.fname b map reg_map current_args) f.blocks;

  Printf.printf ".L_epilogue_%s:\n" f.fname;
  List.iteri (fun i r ->
      let off = -8 - 4 * (slots + i + 1) in
      Printf.printf "    lw %s, %d(fp)\n" r off)
    saved_regs;
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
