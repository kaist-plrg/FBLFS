type t = { target : IOIR.Syn.CallTarget.t; attr : Common.StackSpaceAnnot.t }
[@@deriving sexp]

let pp fmt (p : t) =
  Format.fprintf fmt "call %a [%a];" IOIR.Syn.CallTarget.pp p.target
    Common.StackSpaceAnnot.pp p.attr
