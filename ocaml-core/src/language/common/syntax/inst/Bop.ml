type t =
  | Bpiece
  | Bsubpiece
  | Bint_equal
  | Bint_notequal
  | Bint_less
  | Bint_sless
  | Bint_lessequal
  | Bint_slessequal
  | Bint_add
  | Bint_sub
  | Bint_carry
  | Bint_scarry
  | Bint_sborrow
  | Bint_xor
  | Bint_and
  | Bint_or
  | Bint_left
  | Bint_right
  | Bint_sright
  | Bint_mult
  | Bint_div
  | Bint_rem
  | Bint_sdiv
  | Bint_srem
  | Bbool_xor
  | Bbool_and
  | Bbool_or
  | Bfloat_equal
  | Bfloat_notequal
  | Bfloat_less
  | Bfloat_lessequal
  | Bfloat_add
  | Bfloat_sub
  | Bfloat_mult
  | Bfloat_div
[@@deriving sexp, show { with_path = false }]
