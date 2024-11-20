type t = { entry : JLabel.t; body : Node.t JLabelMap.t } [@@deriving sexp, show]

let of_list (l : (JLabel.t * Node.t) List.t) : t =
  let entry = fst (List.hd l) in
  { entry; body = JLabelMap.of_list l }

let node_num (g : t) : int = JLabelMap.cardinal g.body

let invmap (g : t) : JLabelSet.t JLabelMap.t =
  JLabelMap.fold
    (fun n node acc ->
      let succs = Node.succ node |> JLabelSet.of_list in
      JLabelSet.fold
        (fun succ acc ->
          let old =
            JLabelMap.find_opt succ acc |> Option.value ~default:JLabelSet.empty
          in
          JLabelMap.add succ (JLabelSet.add n old) acc)
        succs acc)
    g.body JLabelMap.empty

let find_interval (g : t) (n : JLabel.t) (invMap : JLabelSet.t JLabelMap.t) :
    JLabelSet.t * JLabelSet.t =
  let interval = JLabelSet.singleton n in
  [%log debug "Finding interval for %a%!" JLabel.pp n];
  let succs = JLabelMap.find n g.body |> Node.succ |> JLabelSet.of_list in
  let rec aux (interval : JLabelSet.t) (succs : JLabelSet.t) :
      JLabelSet.t * JLabelSet.t =
    let newn =
      JLabelSet.filter
        (fun n ->
          JLabelMap.find n invMap |> fun s -> JLabelSet.subset s interval)
        succs
    in
    match JLabelSet.choose_opt newn with
    | None -> (interval, succs)
    | Some n ->
        let interval' = JLabelSet.add n interval in
        let succs' = JLabelSet.remove n succs in
        let succs'' =
          JLabelMap.find n g.body |> Node.succ |> JLabelSet.of_list
        in
        aux interval'
          (JLabelSet.union succs' (JLabelSet.diff succs'' interval'))
  in
  aux interval succs

let mk_interval_graph (g : t) (head : JLabel.t) (addrset : JLabelSet.t)
    (level : Int.t) (idx : Int.t) (new_interval_addr_map : JLabel.t JLabelMap.t)
    : Node.t =
  let nnodes =
    JLabelSet.to_list addrset
    |> List.map (fun n -> (n, JLabelMap.find n g.body))
  in
  let rec aux (nnodes : (JLabel.t * Node.t) List.t) (tmpidx : Int.t)
      (is_loop : Bool.t) (nnodeMap : Node.t JLabelMap.t)
      (exits : JLabel.t List.t) : Bool.t * Node.t JLabelMap.t * JLabel.t List.t
      =
    match nnodes with
    | [] -> (is_loop, nnodeMap, List.rev exits)
    | (n, node) :: nnodes ->
        let (tmpidx, is_loop, exits), nnode =
          Node.fold_label
            (fun (tmpidx, is_loop, exits) l ->
              if JLabel.compare l head = 0 then
                ((tmpidx, true, exits), JLabel.Continue { level; id = idx })
              else if JLabelSet.mem l addrset then ((tmpidx, is_loop, exits), l)
              else (
                assert (JLabelMap.mem l new_interval_addr_map);
                ( ( tmpidx + 1,
                    is_loop,
                    JLabelMap.find l new_interval_addr_map :: exits ),
                  JLabel.Out tmpidx )))
            (tmpidx, is_loop, exits) node
        in
        aux nnodes tmpidx is_loop (JLabelMap.add n nnode nnodeMap) exits
  in
  let is_loop, nnodeMap, exits = aux nnodes 0 false JLabelMap.empty [] in
  if is_loop then Node.LoopN { head; body = nnodeMap; exits }
  else Node.DagN { head; body = nnodeMap; exits }

let interval_graph (g : t) (level : int) : t =
  let entry = g.entry in
  let h = JLabelSet.singleton entry in
  let rec aux (h : JLabelSet.t) (intervals : (JLabel.t * JLabelSet.t) List.t) :
      (JLabel.t * JLabelSet.t) List.t =
    match JLabelSet.choose_opt h with
    | None -> intervals
    | Some n ->
        let invMap = invmap g in
        let interval, succs = find_interval g n invMap in
        let h' = JLabelSet.remove n h in
        let h'' = JLabelSet.union h' succs in
        let h''' =
          JLabelSet.diff h'' (List.map fst intervals |> JLabelSet.of_list)
        in
        aux h''' ((n, interval) :: intervals)
  in
  let intervals = aux h [] in
  let new_interval_addr_map =
    List.map fst intervals
    |> List.mapi (fun i n -> (n, JLabel.Abs { level; id = i }))
    |> JLabelMap.of_list
  in
  let new_entry = JLabelMap.find entry new_interval_addr_map in
  let new_body =
    intervals
    |> List.mapi (fun i (n, addrset) ->
           ( JLabel.Abs { level; id = i },
             mk_interval_graph g n addrset level i new_interval_addr_map ))
    |> JLabelMap.of_list
  in
  { entry = new_entry; body = new_body }
