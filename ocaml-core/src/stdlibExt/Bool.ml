include Stdlib.Bool

let t_of_sexp = Sexplib.Conv.bool_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_bool
let pp fmt v = Format.fprintf fmt "%b" v
