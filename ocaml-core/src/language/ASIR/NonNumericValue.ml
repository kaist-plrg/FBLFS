open Common

type t = SP of SPVal.t | Undef of Int32.t

let pp fmt = function
  | SP sp -> SPVal.pp fmt sp
  | Undef i -> Format.fprintf fmt "undef_%ld" i

let compare (a : t) (b : t) =
  match (a, b) with
  | SP a, SP b -> SPVal.compare a b
  | Undef a, Undef b -> Int32.compare a b
  | SP _, _ -> -1
  | Undef _, _ -> 1

let eval_uop (u : Uop.t) (v : t) (outwidth : Int32.t) :
    (NumericValue.t, t) Either.t =
  Right (Undef outwidth)

let add_sp_arith (o : SPVal.t) (v : Int64.t) : t =
  SP
    {
      timestamp = o.timestamp;
      func = o.func;
      multiplier = o.multiplier;
      offset = Int64.add o.offset v;
    }

let eval_bop (b : Bop.t)
    (vs : (t * t, t * NumericValue.t, NumericValue.t * t) Either3.t)
    (outwidth : Int32.t) : (NumericValue.t, t) Either.t =
  match (b, vs) with
  | _, Second (Undef _, _) | _, Third (_, Undef _) -> Right (Undef outwidth)
  | Bop.Bint_add, Second (SP o, lv) | Bop.Bint_add, Third (lv, SP o) -> (
      match NumericValue.value_64 lv with
      | Ok ln ->
          Right (add_sp_arith o (Int64.sext ln (NumericValue.width lv) 8l))
      | Error _ -> Right (Undef outwidth))
  | Bop.Bint_sub, Second (SP o, rv) -> (
      match NumericValue.value_64 rv with
      | Ok rn ->
          Right
            (add_sp_arith o
               (Int64.neg (Int64.sext rn (NumericValue.width rv) 8l)))
      | Error _ -> Right (Undef outwidth))
  | Bop.Bint_sub, Third (rv, SP o) -> (
      match NumericValue.value_64 rv with
      | Ok rn ->
          Right
            (SP
               {
                 o with
                 multiplier = Int64.neg o.multiplier;
                 offset = Int64.sub rn o.offset;
               })
      | Error _ -> Right (Undef outwidth))
  | Bop.Bint_sub, First (SP o1, SP o2) ->
      if (o1.timestamp, o1.func) = (o2.timestamp, o2.func) then
        if o1.multiplier = o2.multiplier then
          Left (NumericValue.of_int64 (Int64.sub o1.offset o2.offset) outwidth)
        else
          Right
            (SP
               {
                 timestamp = o1.timestamp;
                 func = o1.func;
                 multiplier = Int64.sub o1.multiplier o2.multiplier;
                 offset = Int64.sub o1.offset o2.offset;
               })
      else Right (Undef outwidth)
  | Bop.Bint_sub, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && v1 = v2 then
        Left (NumericValue.zero outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_mult, Second (SP o, lv) | Bop.Bint_mult, Third (lv, SP o) -> (
      match NumericValue.value_64 lv with
      | Ok rn ->
          Right
            (SP
               {
                 o with
                 multiplier = Int64.mul o.multiplier rn;
                 offset = Int64.mul o.offset rn;
               })
      | Error _ -> Right (Undef outwidth))
  | Bop.Bint_xor, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && compare v1 v2 = 0
      then Left (NumericValue.zero outwidth)
      else Right (Undef outwidth)
  | Bop.Bint_and, First (v1, v2) ->
      if (match v1 with Undef _ -> false | _ -> true) && compare v1 v2 = 0
      then Right v1
      else Right (Undef outwidth)
  | Bop.Bint_equal, First (SP o1, SP o2) ->
      if SPVal.compare o1 o2 = 0 then Left (NumericValue.of_int64 1L 1l)
      else Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_equal, Second _ | Bop.Bint_equal, Third _ ->
      Left (NumericValue.of_int64 0L 1l)
  | Bop.Bint_notequal, First (SP o1, SP o2) ->
      if (o1.timestamp, o1.func, o1.offset) = (o2.timestamp, o2.func, o2.offset)
      then Left (NumericValue.of_int64 0L 1l)
      else Left (NumericValue.of_int64 1L 1l)
  | Bop.Bint_notequal, Second _ | Bop.Bint_notequal, Third _ ->
      Left (NumericValue.of_int64 1L 1l)
  | _ -> Right (Undef outwidth)

let width (v : t) : Int32.t = match v with Undef width -> width | _ -> 8l
let undefined (width : Int32.t) : t = Undef width
let sp (v : SPVal.t) : t = SP v

let get_sp (v : t) : SPVal.t Option.t =
  match v with SP v -> Some v | _ -> None
