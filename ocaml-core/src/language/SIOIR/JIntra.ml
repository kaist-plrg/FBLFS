open Common

type t =
  | Junimplemented
  | Jjump of JLabel.t
  | Jjump_ind of {
      target : NumericVarNode.t;
      candidates : (Int64.t * JLabel.t) List.t;
    }
  | Jcbranch of {
      condition : NumericVarNode.t;
      target_true : JLabel.t;
      target_false : JLabel.t;
    }
[@@deriving sexp]

let pp fmt (v : t) =
  match v with
  | Jjump i -> Format.fprintf fmt "goto %a;" JLabel.pp i
  | Jjump_ind { target; candidates; _ } ->
      Format.fprintf fmt "goto *%a (from %a);" NumericVarNode.pp target
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           JLabel.pp)
        (List.map snd candidates)
  | Jcbranch { condition; target_true; target_false } ->
      Format.fprintf fmt "if %a goto %a else goto %a;" NumericVarNode.pp
        condition JLabel.pp target_true JLabel.pp target_false
  | Junimplemented -> Format.fprintf fmt "unimplemented"

let succ (v : t) : JLabel.t List.t =
  match v with
  | Jcbranch { target_true; target_false; _ } -> [ target_true; target_false ]
  | Jjump n -> [ n ]
  | Jjump_ind { candidates; _ } -> List.map snd candidates
  | Junimplemented -> []
