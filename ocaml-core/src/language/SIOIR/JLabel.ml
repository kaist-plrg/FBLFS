open Common

type t =
  | Abs of { level : Int.t; id : Int.t }
  | Concrete of { jmp : Bool.t; loc : Loc.t }
  | Continue of { level : Int.t; id : Int.t }
  | Out of Int.t
[@@deriving show, sexp]

let compare a b =
  match (a, b) with
  | Abs { level = a1; id = a2 }, Abs { level = b1; id = b2 } ->
      if a1 = b1 then a2 - b2 else a1 - b1
  | Concrete { jmp = a1; loc = a2 }, Concrete { jmp = b1; loc = b2 } ->
      if Bool.compare a1 b1 = 0 then Loc.compare a2 b2 else Bool.compare a1 b1
  | Continue { level = a1; id = a2 }, Continue { level = b1; id = b2 } ->
      if a1 = b1 then a2 - b2 else a1 - b1
  | Out a1, Out b1 -> a1 - b1
  | Abs _, _ -> -1
  | _, Abs _ -> 1
  | Concrete _, _ -> -1
  | _, Concrete _ -> 1
  | Continue _, _ -> -1
  | _, Continue _ -> 1

let equal a b = compare a b = 0
