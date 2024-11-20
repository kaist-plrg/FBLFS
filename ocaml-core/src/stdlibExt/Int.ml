include Stdlib.Int

let t_of_sexp = Sexplib.Conv.int_of_sexp
let sexp_of_t = Sexplib.Conv.sexp_of_int
let pp fmt v = Format.fprintf fmt "%d" v
