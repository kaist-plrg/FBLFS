open Common

type t =
  | Call of ICall.t
  | TailCall of ITailCall.t
  | Ret of IRet.t
  | LoadStore of IOIR.Syn.ILoadStore.t
  | SLoadStore of IOIR.Syn.ISLoadStore.t
  | Assignment of IOIR.Syn.IAssignment.t
  | Nop of IOIR.Syn.INop.t
  | Special of IOIR.Syn.ISpecial.t
  | IfElse of (NumericVarNode.t * t * t)
  | Loop of { label : JLabel.t; body : t List.t }
  | Block of { label : JLabel.t Option.t; stmts : t List.t }
  | Switch of { target : NumericVarNode.t; cases : (Int64.t * t) List.t }
  | Break of JLabel.t
  | Continue of JLabel.t
[@@deriving sexp, show]

let rec collect_defs (s : t) : RegIdSet.t =
  match s with
  | Nop _ -> RegIdSet.empty
  | Block { stmts; _ } ->
      List.fold_left
        (fun acc s -> RegIdSet.union acc (collect_defs s))
        RegIdSet.empty stmts
  | Loop { body; _ } ->
      List.fold_left
        (fun acc s -> RegIdSet.union acc (collect_defs s))
        RegIdSet.empty body
  | Break _ | Continue _ -> RegIdSet.empty
  | Switch { cases; _ } ->
      List.fold_left
        (fun acc (_, s) -> RegIdSet.union acc (collect_defs s))
        RegIdSet.empty cases
  | IfElse (cond, t, f) -> RegIdSet.union (collect_defs t) (collect_defs f)
  | Call { target = Cdirect { attr = { outputs; _ }; _ }; _ } ->
      outputs |> RegIdSet.of_list
  | Call { target = Cind _; _ } -> RegIdSet.empty
  | TailCall { target = Cdirect { attr = { outputs; _ }; _ }; _ } ->
      outputs |> RegIdSet.of_list
  | TailCall { target = Cind _; _ } -> RegIdSet.empty
  | Ret r -> RegIdSet.empty
  | LoadStore (Load { output; _ }) -> output.id |> RegIdSet.singleton
  | LoadStore (Store _) -> RegIdSet.empty
  | SLoadStore (Sload { output; _ }) -> output.id |> RegIdSet.singleton
  | SLoadStore (Sstore _) -> RegIdSet.empty
  | Assignment a -> a.output.id |> RegIdSet.singleton
  | Special s -> RegIdSet.empty
