type t = { target : IOIR.Syn.CallTarget.t; attr : IOIR.Syn.TAnnot.t }
[@@deriving sexp]

let pp fmt ({ target; attr } : t) =
  Format.fprintf fmt "tailcall %a [%a];" IOIR.Syn.CallTarget.pp target
    IOIR.Syn.TAnnot.pp attr
