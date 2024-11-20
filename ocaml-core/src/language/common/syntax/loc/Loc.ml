open Sexplib.Std

type t = Int64.t * int

let t_of_sexp = Sexplib.Conv.pair_of_sexp Int64.t_of_sexp_hex int_of_sexp

let sexp_of_t (addr, seq) =
  Sexplib.Conv.sexp_of_pair Int64.sexp_of_t_hex sexp_of_int (addr, seq)

let compare (a : t) (b : t) =
  let c = Int64.compare (fst a) (fst b) in
  if c = 0 then compare (snd a) (snd b) else c

let equal (a : t) (b : t) = compare a b = 0
let hash (addr, seq) = Hashtbl.hash (addr, seq)
let pp fmt (addr, seq) = Format.fprintf fmt "%a:%d" Byte8.pp addr seq
let show = Format.asprintf "%a" pp

let scan (ic : Scanf.Scanning.in_channel) : t =
  Scanf.bscanf ic "%r:%d" Byte8.scan (fun addr seq -> (addr, seq))

let of_addr addr = (addr, 0)
let of_addr_seq = Fun.id
let get_addr = fst
let get_seq = snd
let to_addr_seq = Fun.id
