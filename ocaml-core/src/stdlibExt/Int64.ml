include Stdlib.Int64

let ( let* ) = Result.bind

let to_string_le (x : t) : String.t =
  let rec loop acc i v =
    if i < 0 then acc
    else
      let c = Char.chr (to_int (logand v 0xffL)) in
      loop (c :: acc) (i - 1) (shift_right_logical v 8)
  in
  List.rev (loop [] 7 x) |> List.to_seq |> String.of_seq

let rev_bytes (v : t) (width : int32) : t =
  let rec aux acc v i =
    if i = width then acc
    else
      let byte = Stdlib.Int64.logand v 0xffL in
      let v = Stdlib.Int64.shift_right_logical v 8 in
      let acc = Stdlib.Int64.logor (Stdlib.Int64.shift_left acc 8) byte in
      aux acc v (Int32.add i 1l)
  in
  aux 0L v 0l

let cut_width (v : t) (width : int32) : int64 =
  if width > 7l then v
  else
    Stdlib.Int64.logand v
      (Stdlib.Int64.lognot
         (Stdlib.Int64.shift_left (-1L) (Int32.to_int width * 8)))

let cut_width_bit (v : t) (width : int32) : int64 =
  if width > 63l then v
  else
    Stdlib.Int64.logand v
      (Stdlib.Int64.lognot (Stdlib.Int64.shift_left (-1L) (Int32.to_int width)))

let sext (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Stdlib.Int64.shift_left v (64 - (Int32.to_int in_width * 8)) in
  let x = Stdlib.Int64.shift_right x (64 - (Int32.to_int in_width * 8)) in
  cut_width x out_width

let sext_bit (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Stdlib.Int64.shift_left v (64 - Int32.to_int in_width) in
  let x = Stdlib.Int64.shift_right x (64 - Int32.to_int in_width) in
  cut_width_bit x out_width

let zext (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Stdlib.Int64.shift_left v (64 - (Int32.to_int in_width * 8)) in
  let x =
    Stdlib.Int64.shift_right_logical x (64 - (Int32.to_int in_width * 8))
  in
  cut_width x out_width

let zext_bit (v : t) (in_width : int32) (out_width : int32) : int64 =
  let x = Stdlib.Int64.shift_left v (64 - Int32.to_int in_width) in
  let x = Stdlib.Int64.shift_right_logical x (64 - Int32.to_int in_width) in
  cut_width_bit x out_width

let bitwidth (v : t) : int64 =
  let rec aux acc v =
    if v = 0L then acc
    else aux (Stdlib.Int64.add acc 1L) (Stdlib.Int64.shift_right_logical v 1)
  in
  aux 0L v

let bitcount (v : t) : int64 =
  let rec aux acc v =
    if v = 0L then acc
    else
      aux
        (Stdlib.Int64.add acc (Stdlib.Int64.logand v 1L))
        (Stdlib.Int64.shift_right_logical v 1)
  in
  aux 0L v

let trunc (v : t) (inwidth : int32) (outwidth : int32) : (t, String.t) Result.t
    =
  let* fv =
    if inwidth = 4l then Ok (Int32.float_of_bits (Stdlib.Int64.to_int32 v))
    else if inwidth = 8l then Ok (Stdlib.Int64.float_of_bits v)
    else Error "int2float: unsupported width"
  in
  Ok (cut_width (of_float fv) outwidth)

let concat (v1 : t) (v2 : t) (v1width : int32) (v2width : int32) :
    (t, String.t) Result.t =
  if Int32.add v1width v2width > 8l then Error "concat: width too large"
  else
    Ok
      (Stdlib.Int64.logor
         (Stdlib.Int64.shift_left v1 (Int32.to_int v2width))
         v2)

let carry (l : t) (r : t) (width : int32) : bool =
  if width = 8l then Stdlib.Int64.unsigned_compare (Stdlib.Int64.add l r) l < 0
  else
    Stdlib.Int64.add l r >= Stdlib.Int64.shift_left 1L (Int32.to_int width * 8)

let scarry (l : t) (r : t) (width : int32) : bool =
  if width = 8l then
    (l > 0L && r > 0L && Stdlib.Int64.add l r < 0L)
    || (l < 0L && r < 0L && Stdlib.Int64.add l r >= 0L)
  else
    let msbl =
      Stdlib.Int64.logand
        (Stdlib.Int64.shift_right l ((Int32.to_int width * 8) - 1))
        1L
    in
    let msbr =
      Stdlib.Int64.logand
        (Stdlib.Int64.shift_right r ((Int32.to_int width * 8) - 1))
        1L
    in
    let msb =
      Stdlib.Int64.logand
        (Stdlib.Int64.shift_right (Stdlib.Int64.add l r)
           ((Int32.to_int width * 8) - 1))
        1L
    in
    (msbl = 1L && msbr = 1L && msb = 0L) || (msbl = 0L && msbr = 0L && msb = 1L)

let sborrow (l : t) (r : t) (width : int32) : bool =
  scarry l (cut_width (Stdlib.Int64.neg r) width) width

let pp fmt v = Format.fprintf fmt "%Lx" v
let show = Format.asprintf "%a" pp
let t_of_sexp = Sexplib.Conv.int64_of_sexp
let sexp_of_t v = Sexplib.Conv.sexp_of_int64 v

let t_of_sexp_hex (s : Sexplib.Sexp.t) : t =
  match s with
  | Sexplib.Sexp.Atom s -> Scanf.sscanf s "%Lx" Fun.id
  | _ -> Sexplib.Conv_error.no_variant_match ()

let sexp_of_t_hex (v : t) : Sexplib.Sexp.t =
  Sexplib.Sexp.Atom (Format.sprintf "%Lx" v)
