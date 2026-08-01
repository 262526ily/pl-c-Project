(* lib/const_table.ml
 * 独立的常量值表，供 semantic.ml 和 ir.ml 共享
 *)

(* 常量值表：记录编译期已知的常量值 *)
let const_value_table : (string, int) Hashtbl.t = Hashtbl.create 32

(* 记录常量值 *)
let record_const_value name value =
  Hashtbl.replace const_value_table name value

(* 查询常量值 *)
let get_const_value name =
  try Some (Hashtbl.find const_value_table name)
  with Not_found -> None

(* 判断是否为常量 *)
let is_const_name name =
  Hashtbl.mem const_value_table name

(* 清空常量表（用于多文件编译） *)
let clear () =
  Hashtbl.clear const_value_table