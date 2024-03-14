module NonNumericValue = NonNumericValue
module Cont = Common.ContF.Make (Inst) (Jmp) (Block) (Func) (Prog)
module Value = Common.ValueF.Make (NonNumericValue)
module TimeStamp = Common.Int64TimeStamp
module Cursor = Common.CursorF.Make (TimeStamp)
module RegFile = Common.RegFileF.Make (Value)
module Frame = Common.FrameF.Make (Value)
module LocalMemory = Common.LocalMemoryF.Make (Value) (Frame)
module Memory = Common.MemoryF.Make (Value)

module Store =
  Common.HighStoreF.Make (Prog) (Value) (Cursor) (RegFile) (Memory) (Frame)
    (LocalMemory)

module Stack = struct
  open Common

  type elem_t = Cursor.t * Value.t * Loc.t
  type t = elem_t List.t

  let pp (fmt : Format.formatter) (v : t) : unit =
    let pp_elem_t (fmt : Format.formatter) (v : elem_t) : unit =
      let (c, v, loc') : elem_t = v in
      Format.fprintf fmt "(%a, %a, %a)" Cursor.pp c Value.pp v Loc.pp loc'
    in
    Format.fprintf fmt "[%a]" (Format.pp_print_list pp_elem_t) v

  let get_cursor (v : elem_t) : Cursor.t =
    let c, _, _ = v in
    c

  let get_fallthrough (v : elem_t) : Loc.t =
    let _, _, loc = v in
    loc
end

module State =
  Common.HighStateF.Make (Func) (Prog) (CallTarget) (JCall) (JRet) (TimeStamp)
    (Value)
    (Store)
    (Cont)
    (Cursor)
    (Stack)
    (World.Environment)
