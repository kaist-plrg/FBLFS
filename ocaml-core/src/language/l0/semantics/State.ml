open Basic
open Basic_collection

type t = { regs : RegFile.t; mem : Memory.t; pc : Loc.t }

let add_reg (s : t) (r : RegId.t) (v : Value.t) : t =
  { s with regs = RegFile.add_reg s.regs r v }

let get_reg (s : t) (r : RegId.t) : Value.t = RegFile.get_reg s.regs r

let load_mem (s : t) (addr : Addr.t) (width : Int32.t) : Value.t =
  Memory.load_mem s.mem addr width

let store_mem (s : t) (addr : Addr.t) (v : Value.t) : t =
  { s with mem = Memory.store_mem s.mem addr v }

let pp fmt (s : t) : unit = Format.fprintf fmt "pc: %a\n" Loc.pp s.pc
