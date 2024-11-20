include Map.Make (JLabel)

let pp f fmt m =
  let pp_item fmt (k, v) = Format.fprintf fmt "%a -> %a" JLabel.pp k f v in
  Format.fprintf fmt "{%a}" (Format.pp_print_list pp_item) (bindings m)

let t_of_sexp f v =
  Sexplib.Conv.list_of_sexp
    (function
      | List [ k; v ] -> (JLabel.t_of_sexp k, f v)
      | _ -> Sexplib.Conv_error.no_variant_match ())
    v
  |> of_list

let sexp_of_t f v =
  Sexplib.Conv.sexp_of_list
    (fun (k, v) -> List [ JLabel.sexp_of_t k; f v ])
    (bindings v)
