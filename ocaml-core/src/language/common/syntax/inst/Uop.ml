type t =
  | Upopcount
  | Ulzcount
  | Uint_zext
  | Uint_sext
  | Uint_2comp
  | Uint_negate
  | Ubool_negate
  | Ufloat_neg
  | Ufloat_abs
  | Ufloat_sqrt
  | Ufloat_ceil
  | Ufloat_floor
  | Ufloat_round
  | Ufloat_nan
  | Uint2float
  | Ufloat2float
  | Utrunc
[@@deriving sexp, show]
