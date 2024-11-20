type t = { attr : IOIR.Syn.RAnnot.t } [@@deriving sexp]

let pp fmt ({ attr } : t) =
  Format.fprintf fmt "return [%a];" IOIR.Syn.RAnnot.pp attr
