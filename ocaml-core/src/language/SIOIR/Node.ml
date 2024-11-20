type t =
  | LoopN of { head : JLabel.t; body : t JLabelMap.t; exits : JLabel.t List.t }
  | DagN of { head : JLabel.t; body : t JLabelMap.t; exits : JLabel.t List.t }
  | StmtN of { stmts : Stmt.t List.t; next : JLabel.t Option.t }
  | JmpN of JIntra.t
[@@deriving sexp, show]

let succ = function
  | LoopN { exits; _ } -> exits
  | DagN { exits; _ } -> exits
  | StmtN { next; _ } -> Option.to_list next
  | JmpN j -> JIntra.succ j

let fold_label (f : 'acc -> JLabel.t -> 'acc * JLabel.t) (init : 'acc) (n : t) :
    'acc * t =
  match n with
  | LoopN { head; body; exits } ->
      let nexit, acc =
        List.fold_left
          (fun ((nexit, acc) : JLabel.t List.t * 'acc) (e : JLabel.t) ->
            let acc, e = f acc e in
            (e :: nexit, acc))
          ([], init) exits
      in
      (acc, LoopN { head; body; exits = List.rev nexit })
  | DagN { head; body; exits } ->
      let nexit, acc =
        List.fold_left
          (fun ((nexit, acc) : JLabel.t List.t * 'acc) (e : JLabel.t) ->
            let acc, e = f acc e in
            (e :: nexit, acc))
          ([], init) exits
      in
      (acc, DagN { head; body; exits = List.rev nexit })
  | StmtN { stmts; next } -> (
      match next with
      | None -> (init, n)
      | Some l ->
          let acc, l = f init l in
          (acc, StmtN { stmts; next = Some l }))
  | JmpN j -> (
      match j with
      | Jjump l ->
          let acc, l = f init l in
          (acc, JmpN (JIntra.Jjump l))
      | Jjump_ind { target; candidates } ->
          let ncandidates, acc =
            List.fold_left
              (fun ((ncandidates, acc) : (Int64.t * JLabel.t) List.t * 'acc)
                   (c : Int64.t * JLabel.t) ->
                let i, l = c in
                let acc, l = f acc l in
                ((i, l) :: ncandidates, acc))
              ([], init) candidates
          in
          ( acc,
            JmpN
              (JIntra.Jjump_ind { target; candidates = List.rev ncandidates })
          )
      | Junimplemented -> (init, n)
      | Jcbranch { condition; target_true; target_false } ->
          let acc, new_true = f init target_true in
          let acc, new_false = f acc target_false in
          ( acc,
            JmpN
              (JIntra.Jcbranch
                 { condition; target_true = new_true; target_false = new_false })
          ))
