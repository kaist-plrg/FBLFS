open Common

type t = {
  sp_num : Int32.t;
  fp_num : Int32.t;
  funcs : Func.t List.t;
  rom : DMem.t; [@opaque]
  rspec : Int32.t Int32Map.t; [@opaque]
  externs : String.t Byte8Map.t; [@opaque]
  objects : (Int64.t * String.t) List.t; [@opaque]
}
[@@deriving sexp, show, fields]

let get_func_opt (p : t) (loc : Loc.t) : Func.t option =
  List.find_opt (fun (f : Func.t) -> Loc.compare f.entry loc = 0) p.funcs
