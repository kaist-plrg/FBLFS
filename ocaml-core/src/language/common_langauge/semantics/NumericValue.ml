open StdlibExt
open Basic

type t = { value : Int64.t; width : Int32.t }

let zero width = { value = Int64.zero; width }
let isZero x = Int64.equal x.value Int64.zero
let to_loc (x : t) : Loc.t = (x.value, 0)
let to_addr (x : t) : Addr.t = x.value
let pp fmt (x : t) = Format.fprintf fmt "%Lx:%ld" x.value x.width

let of_chars (cs : Char.t list) : t =
  let rec loop acc = function
    | [] -> acc
    | c :: cs ->
        let acc = Int64.shift_left acc 8 in
        let acc = Int64.logor acc (Int64.of_int (Char.code c)) in
        loop acc cs
  in
  let value = loop Int64.zero (List.rev cs) in
  let width = Int32.of_int (List.length cs) in
  { value; width }

let to_chars (x : t) : Char.t list =
  let rec loop acc i v =
    if i < 0 then acc
    else
      let c = Char.chr (Int64.to_int (Int64.logand v 0xffL)) in
      loop (c :: acc) (i - 1) (Int64.shift_right_logical v 8)
  in
  List.rev (loop [] (Int32.to_int x.width - 1) x.value)

let of_int64 v width =
  let bits = Int64Ext.bitwidth v in
  if Int64.to_int32 bits > Int32.mul width 8l then
    raise
      (Invalid_argument
         (Format.sprintf "NumericValue.of_int64: %Ld does not fit in %ld bytes"
            v width))
  else { value = v; width }

let of_int64_safe (v : Int64.t) (width : Int32.t) : (t, String.t) Result.t =
  let bits = Int64Ext.bitwidth v in
  if Int64.to_int32 bits > Int32.mul width 8l then
    Error
      (Format.sprintf
         "NumericValue.of_int64_safe: %Ld does not fit in %ld bytes" v width)
  else Ok { value = v; width }

let refine_width (x : t) (width : Int32.t) : t =
  if Int32.equal x.width width then x
  else { value = Int64Ext.cut_width x.value width; width }

let replace_width (ov : t) (nv : t) (width : Int32.t) : t =
  let rv =
    if Int32.equal width 8l then Int64.zero
    else
      Int64.logand ov.value
        (Int64.shift_left (-1L) (Int32.to_int (Int32.mul width 8l)))
  in
  {
    value = Int64.logor rv (Int64Ext.cut_width nv.value width);
    width = ov.width;
  }
