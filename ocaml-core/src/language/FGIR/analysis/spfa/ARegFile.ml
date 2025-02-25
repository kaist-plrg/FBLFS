open Common

type t = TopHoleMap of AbsVal.t RegIdMap.t | Bottom

let pp fmt = function
  | TopHoleMap m ->
      RegIdMap.iter
        (fun k v -> Format.fprintf fmt "%a: %a" RegId.pp k AbsVal.pp v)
        m
  | Bottom -> Format.fprintf fmt "Bottom"

let join (c1 : t) (c2 : t) : t =
  match (c1, c2) with
  | TopHoleMap m1, TopHoleMap m2 ->
      TopHoleMap
        (RegIdMap.merge
           (fun _ s1 s2 ->
             match (s1, s2) with
             | Some s1, Some s2 -> Some (AbsVal.join s1 s2)
             | _, _ -> None)
           m1 m2)
  | Bottom, v | v, Bottom -> v

let le (c1 : t) (c2 : t) : bool =
  match (c1, c2) with
  | TopHoleMap m1, TopHoleMap m2 ->
      RegIdMap.for_all
        (fun r s2 ->
          let s1 = RegIdMap.find_opt r m1 |> Option.value ~default:AbsVal.Top in
          AbsVal.le s1 s2)
        m2
  | Bottom, _ -> true
  | _, Bottom -> false

let add (c : t) (r : RegId.t) (s : AbsVal.t) : t =
  match c with
  | TopHoleMap m -> TopHoleMap (RegIdMap.add r s m)
  | Bottom -> Bottom

let get (c : t) (r : RegId.t) : AbsVal.t =
  match c with
  | TopHoleMap m -> RegIdMap.find_opt r m |> Option.value ~default:AbsVal.Top
  | Bottom -> AbsVal.bottom

let top = TopHoleMap RegIdMap.empty
