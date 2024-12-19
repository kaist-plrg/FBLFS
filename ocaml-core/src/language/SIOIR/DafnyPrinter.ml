(*
   let rec print_dafny fmt (node : Node.t) : Unit.t =
     match node with
     | LoopN { head; body; exits } ->
         Format.fprintf fmt "LoopN { %a }" print_dafny_nodes (head, body)
     | _ -> Node.pp fmt node

   and print_dafny_nodes fmt ((entry, body) : JLabel.t * Node.t JLabelMap.t) :
       Unit.t =
     let ilist = [ (entry, JLabelMap.find entry body) ] in
     let invMap = Graph.invmap { entry; body } in
     let interval = JLabelSet.singleton entry in
     [%log debug "Finding interval for %a%!" JLabel.pp entry];
     let succs = JLabelMap.find entry body |> Node.succ |> JLabelSet.of_list in
     let rec aux (ilist : (JLabel.t * Node.t) List.t) (interval : JLabelSet.t)
         (succs : JLabelSet.t) :
         (JLabel.t * Node.t) List.t * JLabelSet.t * JLabelSet.t =
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
           aux ((n, node) :: ilist) interval'
             (JLabelSet.union succs' (JLabelSet.diff succs'' interval'))
     in
     let ilist, _, _ = aux ilist interval succs in
     List.iter
       (fun (l, n) ->
         Format.fprintf fmt "@[<hov 2>%a:@;%a@]@;" JLabel.pp l print_dafny n)
       ilist

   let print_dafny_graph fmt (g : Graph.t) : Unit.t =
     Format.fprintf fmt "@[%a@]" print_dafny_nodes (g.entry, g.body)
*)
let print_dafny_label fmt (l : JLabel.t) : Unit.t =
  match l with
  | Concrete { jmp; loc } ->
      Format.fprintf fmt "LC_%b_%Lx_%d" jmp (Common.Loc.get_addr loc)
        (Common.Loc.get_seq loc)
  | Abs { level; id } -> Format.fprintf fmt "LA_%d_%d" level id
  | Continue { level; id } -> Format.fprintf fmt "LL_%d_%d" level id
  | Out i -> failwith "unreachable"

let print_dafny_regid fmt (r : Common.RegId.t) : Unit.t =
  match r with
  | Unique i -> Format.fprintf fmt "U%lx" i
  | Register r -> Format.fprintf fmt "R%lx" r

let print_dafny_varnode fmt (v : Common.NumericVarNode.t) : Unit.t =
  match v with
  | Register r -> print_dafny_regid fmt r.id
  | Const i -> Format.fprintf fmt "Value.Const(%Ld, %ld)" i.value i.width
  | Ram (i, w) -> Format.fprintf fmt "unhandled_ram_%ld" w

let print_dafny_assignment fmt (a : IOIR.Syn.IAssignment.t) : Unit.t =
  Format.fprintf fmt "%a := " print_dafny_regid a.output.id;
  match a.expr with
  | Avar v ->
      print_dafny_varnode fmt v;
      Format.fprintf fmt ";"
  | Auop (op, v) ->
      Format.fprintf fmt "Value.%s(%a,%ld);" (Common.Uop.show op)
        print_dafny_varnode v a.output.width
  | Abop (op, v1, v2) ->
      Format.fprintf fmt "Value.%s(%a,%a,%ld);" (Common.Bop.show op)
        print_dafny_varnode v1 print_dafny_varnode v2 a.output.width

let print_dafny_sloadstore fmt (ls : IOIR.Syn.ISLoadStore.t) : Unit.t =
  match ls with
  | Sload { offset; output } ->
      Format.fprintf fmt "%a := Memory.FromStack(mem, %Ld, %ld);"
        print_dafny_regid output.id offset output.width
  | Sstore { offset; value } ->
      Format.fprintf fmt "mem := Memory.AssignStack(mem, %Ld, %a, %ld);" offset
        print_dafny_varnode value
        (Common.NumericVarNode.get_width value)

let print_dafny_loadstore fmt (ls : IOIR.Syn.ILoadStore.t) : Unit.t =
  match ls with
  | Load { output; pointer; space } ->
      Format.fprintf fmt "%a := Memory.Load(mem, %a, %a, %ld);"
        print_dafny_regid output.id print_dafny_varnode pointer
        print_dafny_varnode space output.width
  | Store { pointer; value; space } ->
      Format.fprintf fmt "mem := Memory.Store(mem, %a, %a, %a,%ld);"
        print_dafny_varnode pointer print_dafny_varnode value
        print_dafny_varnode space
        (Common.NumericVarNode.get_width value)

let print_dafny_return fmt (r : IRet.t) : Unit.t = Format.fprintf fmt "return;"

let rec print_dafny_stmt fmt (s : Stmt.t) : Unit.t =
  match s with
  | Nop INop -> ()
  | Block { label; stmts } -> (
      match label with
      | None ->
          Format.fprintf fmt "@[<v 1>{@;%a@;}@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_cut print_dafny_stmt)
            stmts
      | Some label ->
          Format.fprintf fmt "@[<v 1>label %a: {@;%a@;}@]" print_dafny_label
            label
            (Format.pp_print_list ~pp_sep:Format.pp_print_cut print_dafny_stmt)
            stmts)
  | Loop { label; body } ->
      Format.fprintf fmt "@[<v 1>label %a: while true {@;%a@;}@]"
        print_dafny_label label
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut print_dafny_stmt)
        body
  | Break label -> Format.fprintf fmt "break %a;" print_dafny_label label
  | Continue label -> Format.fprintf fmt "continue %a;" print_dafny_label label
  | Switch { target; cases } ->
      Format.fprintf fmt "@[<v 1>match %a {@;%a@;}@]" print_dafny_varnode target
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut
           (fun fmt (idx, stmt) ->
             Format.fprintf fmt "case %Ld => %a" idx print_dafny_stmt stmt))
        cases
  | IfElse (cond, t, f) ->
      Format.fprintf fmt
        "@[<v 1>if Value.IsNonZero(%a) {@;%a@;} else {@;%a@;}@]"
        print_dafny_varnode cond print_dafny_stmt t print_dafny_stmt f
  | Call c -> ICall.pp fmt c
  | TailCall c -> ITailCall.pp fmt c
  | Ret r -> print_dafny_return fmt r
  | LoadStore ls -> print_dafny_loadstore fmt ls
  | SLoadStore ls -> print_dafny_sloadstore fmt ls
  | Assignment a -> print_dafny_assignment fmt a
  | Special s -> IOIR.Syn.ISpecial.pp fmt s

let print_dafny_decl fmt (r : Common.RegId.t) : Unit.t =
  Format.fprintf fmt "%a: Value.T" print_dafny_regid r

let print_dafny_decl_input fmt (r : Common.RegId.t) : Unit.t =
  Format.fprintf fmt "%a_i: Value.T" print_dafny_regid r

let print_dafny_decl_set fmt (d : Common.RegIdSet.t) : Unit.t =
  if Common.RegIdSet.is_empty d then ()
  else
    Format.fprintf fmt "@[<hov 1>var %a;@;@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",@ ")
         print_dafny_decl)
      (Common.RegIdSet.elements d)

let print_dafny_copy_input fmt (inputs : Common.RegIdSet.t) : Unit.t =
  Format.fprintf fmt "%a@;"
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt "@;")
       (fun fmt r ->
         Format.fprintf fmt "%a := %a_i;" print_dafny_regid r print_dafny_regid
           r))
    (inputs |> Common.RegIdSet.elements)

let print_dafny_assign_undef fmt (undefs : Common.RegIdSet.t) : Unit.t =
  Format.fprintf fmt "%a@;"
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt "@;")
       (fun fmt r ->
         Format.fprintf fmt "%a := Value.Undefined(1);" print_dafny_regid r))
    (undefs |> Common.RegIdSet.elements)

let print_dafny_func fmt (f : Func.t) : unit =
  let inputSet = Common.RegIdSet.of_list f.attr.inputs in
  let defSet = f.body |> Stmt.collect_defs in
  let outputSet = Common.RegIdSet.of_list f.attr.outputs in
  Format.fprintf fmt
    "@[<v 1>method %s(%a) returns (%a) {@;%a@;%a@;%a@;mem := mem_i;@;%a@;}@]"
    (f.nameo
    |> Option.value ~default:(f.entry |> Common.Loc.get_addr |> Int64.show))
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",@ ")
       (fun fmt r -> Format.fprintf fmt "%s" r))
    ([ "mem_i: Memory.T" ]
    @ (f.attr.inputs
      |> List.map (fun r -> Format.asprintf "%a" print_dafny_decl_input r)))
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt ",@ ")
       (fun fmt r -> Format.fprintf fmt "%s" r))
    ([ "mem: Memory.T" ]
    @ (f.attr.outputs
      |> List.map (fun r -> Format.asprintf "%a" print_dafny_decl r)))
    print_dafny_decl_set
    (Common.RegIdSet.diff (Common.RegIdSet.union defSet inputSet) outputSet)
    print_dafny_copy_input inputSet print_dafny_assign_undef
    (Common.RegIdSet.diff (Common.RegIdSet.union defSet outputSet) inputSet)
    print_dafny_stmt f.body
