let restruct (g : Graph.t) : Graph.t =
  let rec aux (g : Graph.t) (level : int) : Graph.t =
    let nlen = Graph.node_num g in
    [%log debug "Restructuring level %d with %d nodes" level nlen];
    let g' = Graph.interval_graph g level in
    if Graph.node_num g' = nlen then
      if nlen = 1 then g
      else (
        (* node duplication *)
        [%log info "Translation failed"];
        Graph.of_list
          [
            ( JLabel.Abs { level; id = 0 },
              Node.StmtN
                { stmts = [ Stmt.Special "translationfailed" ]; next = None } );
          ])
    else aux g' (level + 1)
  in
  aux g 0

let remove_out (g : Graph.t) : Graph.t =
  let rec aux (body : Node.t JLabelMap.t) : Node.t JLabelMap.t =
    JLabelMap.map
      (fun n ->
        match n with
        | Node.DagN { head; body; exits } ->
            let body =
              JLabelMap.map
                (fun n ->
                  Node.fold_label
                    (fun () l ->
                      match l with
                      | JLabel.Out idx -> ((), List.nth exits idx)
                      | _ -> ((), l))
                    () n
                  |> snd)
                body
            in
            Node.DagN { head; body = aux body; exits }
        | Node.LoopN { head; body; exits } ->
            let body =
              JLabelMap.map
                (fun n ->
                  Node.fold_label
                    (fun () l ->
                      match l with
                      | JLabel.Out idx -> ((), List.nth exits idx)
                      | _ -> ((), l))
                    () n
                  |> snd)
                body
            in
            Node.LoopN { head; body = aux body; exits }
        | _ -> n)
      body
  in
  { g with body = aux g.body }

let reduce_dag_target (g : Graph.t) : Graph.t =
  let rec aux (l : JLabel.t) (node : Node.t)
      (dagtarget_map : JLabel.t JLabelMap.t) : JLabel.t JLabelMap.t =
    match node with
    | Node.DagN { head; body; exits } ->
        let dagtarget_map = JLabelMap.fold aux body dagtarget_map in
        let target =
          JLabelMap.find_opt head dagtarget_map |> Option.value ~default:head
        in
        JLabelMap.add l target dagtarget_map
    | Node.LoopN { head; body; exits } -> JLabelMap.fold aux body dagtarget_map
    | _ -> dagtarget_map
  in
  let dagtarget_map = JLabelMap.fold aux g.body JLabelMap.empty in
  let rec aux (body : Node.t JLabelMap.t) : Node.t JLabelMap.t =
    JLabelMap.map
      (fun n ->
        let n' =
          Node.fold_label
            (fun () l ->
              ( (),
                match JLabelMap.find_opt l dagtarget_map with
                | Some l -> l
                | None -> l ))
            () n
          |> snd
        in
        match n' with
        | Node.DagN { head; body; exits } ->
            Node.DagN
              {
                head =
                  JLabelMap.find_opt head dagtarget_map
                  |> Option.value ~default:head;
                body = aux body;
                exits =
                  List.map
                    (fun l ->
                      JLabelMap.find_opt l dagtarget_map
                      |> Option.value ~default:l)
                    exits;
              }
        | Node.LoopN { head; body; exits } ->
            Node.LoopN
              {
                head =
                  JLabelMap.find_opt head dagtarget_map
                  |> Option.value ~default:head;
                body = aux body;
                exits =
                  List.map
                    (fun l ->
                      JLabelMap.find_opt l dagtarget_map
                      |> Option.value ~default:l)
                    exits;
              }
        | _ -> n')
      body
  in
  {
    entry =
      JLabelMap.find_opt g.entry dagtarget_map |> Option.value ~default:g.entry;
    body = aux g.body;
  }

let remove_dag (g : Graph.t) : Graph.t =
  let rec aux (body : Node.t JLabelMap.t) : Node.t JLabelMap.t =
    JLabelMap.fold
      (fun l n acc ->
        match n with
        | Node.DagN { head; body; exits } ->
            let acc2 = aux body in
            JLabelMap.union (fun _ _ _ -> failwith "duplicate key") acc acc2
        | Node.LoopN { head; body; exits } ->
            let acc2 = aux body in
            JLabelMap.add l (Node.LoopN { head; body = acc2; exits }) acc
        | _ -> JLabelMap.add l n acc)
      body JLabelMap.empty
  in
  { g with body = aux g.body }

let jlabel_to_stmt (j : JLabel.t) : Stmt.t =
  match j with Continue _ -> Stmt.Continue j | _ -> Stmt.Break j

let to_stmt (g : Graph.t) : Stmt.t =
  let rec node_to_stmt (label : JLabel.t) (n : Node.t) : Stmt.t List.t =
    match n with
    | Node.LoopN { head; body } -> (
        match label with
        | JLabel.Abs { level; id } ->
            [
              Stmt.Loop
                {
                  label = JLabel.Continue { level; id };
                  body = [ node_graph_to_stmt (head, body) ];
                };
            ]
        | _ -> assert false)
    | Node.StmtN { stmts; next } -> (
        match next with
        | Some next -> stmts @ [ jlabel_to_stmt next ]
        | None -> stmts)
    | Node.JmpN (Jjump target) -> [ jlabel_to_stmt target ]
    | Node.JmpN (Jcbranch { condition; target_true; target_false }) ->
        [
          Stmt.IfElse
            (condition, jlabel_to_stmt target_true, jlabel_to_stmt target_false);
        ]
    | Node.JmpN Junimplemented -> [ Stmt.Special "unimplemented" ]
    | Node.JmpN (Jjump_ind { target; candidates }) ->
        [
          Stmt.Switch
            {
              target;
              cases =
                candidates
                |> List.map (fun (idx, target) -> (idx, jlabel_to_stmt target));
            };
        ]
    | Node.DagN _ -> failwith "unreachable"
  and node_graph_to_stmt ((entry, body) : JLabel.t * Node.t JLabelMap.t) :
      Stmt.t =
    let ilist =
      [ (Some entry, JLabelMap.find entry body |> node_to_stmt entry) ]
    in
    let invMap = Graph.invmap { entry; body } in
    let interval = JLabelSet.singleton entry in
    [%log debug "Finding interval for %a%!" JLabel.pp entry];
    let succs = JLabelMap.find entry body |> Node.succ |> JLabelSet.of_list in
    let rec aux (ilist : (JLabel.t Option.t * Stmt.t List.t) List.t)
        (interval : JLabelSet.t) (succs : JLabelSet.t) :
        (JLabel.t Option.t * Stmt.t List.t) List.t * JLabelSet.t * JLabelSet.t =
      let newn =
        JLabelSet.filter
          (fun n ->
            JLabelMap.find n invMap |> fun s -> JLabelSet.subset s interval)
          succs
      in
      match JLabelSet.choose_opt newn with
      | None -> (List.rev ilist, interval, succs)
      | Some n ->
          let interval' = JLabelSet.add n interval in
          let succs' = JLabelSet.remove n succs in
          let node = JLabelMap.find n body in
          let succs'' =
            node |> Node.succ
            |> List.filter (fun l -> JLabelMap.mem l body)
            |> JLabelSet.of_list
          in
          aux
            ((Some n, node |> node_to_stmt n) :: ilist)
            interval'
            (JLabelSet.union succs' (JLabelSet.diff succs'' interval'))
    in
    let ilist, _, _ = aux ilist interval succs in
    let block, final_after =
      List.fold_left
        (fun (blk, after) (l, stmts) ->
          (Stmt.Block { label = l; stmts = blk :: after }, stmts))
        (Stmt.Break (List.nth ilist 0 |> fst |> Option.get), [])
        ilist
    in
    Stmt.Block { label = None; stmts = block :: final_after }
  in
  node_graph_to_stmt (g.entry, g.body)
