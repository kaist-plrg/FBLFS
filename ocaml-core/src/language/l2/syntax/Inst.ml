open Basic

type t =
  | Iload of (VarNode.t * VarNode.t * RegId.t_full)
  | Istore of (VarNode.t * VarNode.t * VarNode.t)
  | Isload of (Const.t * RegId.t_full)
  | Isstore of (Const.t * VarNode.t)
  | Iassignment of (Assignable.t * RegId.t_full)
  | INop

type t_full = { ins : t; loc : Loc.t; mnem : Mnemonic.t }

let pp (fmt : Format.formatter) (p : t) =
  match p with
  | Iload (i0, i1, o) ->
      Format.fprintf fmt "%a = *[%a]%a;" RegId.pp_full o VarNode.pp i0
        VarNode.pp i1
  | Istore (i0, i1, i2) ->
      Format.fprintf fmt "*[%a]%a = %a;" VarNode.pp i0 VarNode.pp i1 VarNode.pp
        i2
  | Isload (i, o) ->
      Format.fprintf fmt "%a = stack[%a];" RegId.pp_full o Const.pp i
  | Isstore (i, o) ->
      Format.fprintf fmt "stack[%a] = %a;" Const.pp i VarNode.pp o
  | Iassignment (i, o) ->
      Format.fprintf fmt "%a = %a;" RegId.pp_full o Assignable.pp i
  | INop -> Format.fprintf fmt "nop;"

let pp_full (fmt : Format.formatter) (p : t_full) =
  Format.fprintf fmt "%a: %a [%a]" Loc.pp p.loc pp p.ins Mnemonic.pp p.mnem
