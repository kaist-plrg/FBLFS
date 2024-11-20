open Common

type t = {
  nameo : String.t Option.t;
  entry : Loc.t;
  body : Stmt.t;
  attr : IOIR.Syn.Func.Attr.t;
}
[@@deriving sexp, show, fields]
