open Common
open Basic_domain
open Syn
module ICFG = ICFGF.Make (Block)

let has_mem (a : Assignable.t) : bool =
  match a with
  | Avar (Ram _) | Auop (_, Ram _) | Abop (_, _, Ram _) | Abop (_, Ram _, _) ->
      true
  | _ -> false

module RegAnalysisDomain = struct
  type t = RegIdSet.t Option.t
  type data = RegIdSet.t Option.t
  type edge = ICFG.G.E.t
  type vertex = ICFG.G.V.t
  type g = ICFG.G.t

  let direction : Graph.Fixpoint.direction = Backward

  let join (a : t) (b : t) : t =
    match (a, b) with
    | Some a, Some b -> Some (RegIdSet.union a b)
    | None, Some a | Some a, None -> Some a
    | None, None -> None

  let equal (a : t) (b : t) : bool = Option.equal RegIdSet.equal a b

  let gather_regs (a : VarNode.t List.t) : RegId.t List.t =
    let f (a : VarNode.t) : RegId.t Option.t =
      match a with Register x -> Some x.id | _ -> None
    in
    List.filter_map f a

  let gather_regs_assignable (a : Assignable.t List.t) : RegId.t List.t =
    let f (a : Assignable.t) : RegId.t List.t =
      match a with
      | Avar e -> gather_regs [ e ]
      | Abop (_, x, y) -> gather_regs [ x; y ]
      | Auop (_, x) -> gather_regs [ x ]
    in
    List.flatten (List.map f a)

  let compute_used_regs_inst (a : RegIdSet.t) (i : Inst.t_full) : RegIdSet.t =
    let gen =
      match i.ins with
      | First (Load { output; pointer; space }) ->
          if RegIdSet.mem output.id a then
            gather_regs [ pointer; space ] |> RegIdSet.of_list
          else RegIdSet.empty
      | First (Store { pointer; value; space }) ->
          gather_regs [ pointer; value; space ] |> RegIdSet.of_list
      | Second (Sload _) -> RegIdSet.empty
      | Second (Sstore { value; _ }) ->
          gather_regs [ value ] |> RegIdSet.of_list
      | Third { output; expr } ->
          if RegIdSet.mem output.id a then
            gather_regs_assignable [ expr ] |> RegIdSet.of_list
          else RegIdSet.empty
      | Fourth INop -> RegIdSet.empty
      | Fifth "syscall" ->
          RegIdSet.of_list
            [
              Register 16l (* RDX *);
              Register 48l (* RSI *);
              Register 56l (* RDI *);
              Register 128l (* R8 *);
              Register 136l (* R9 *);
              Register 144l (* R10 *);
            ]
      | Fifth _ -> RegIdSet.empty
    in
    let kill =
      match i.ins with
      | First (Load { output; pointer; space }) -> RegIdSet.singleton output.id
      | First (Store { pointer; value; space }) -> RegIdSet.empty
      | Second (Sload { output; _ }) -> RegIdSet.singleton output.id
      | Second (Sstore _) -> RegIdSet.empty
      | Third { output; expr } -> RegIdSet.singleton output.id
      | Fourth INop -> RegIdSet.empty
      | Fifth "syscall" -> RegIdSet.singleton (Register 0l (* RAX *))
      | Fifth _ -> RegIdSet.empty
    in
    RegIdSet.union (RegIdSet.diff a kill) gen

  let compute_used_regs_jmp (a : RegIdSet.t) (j : Jmp.t_full) : RegIdSet.t =
    let gen =
      match j.jmp with
      | JC { target = Cdirect { attr }; _ } -> RegIdSet.of_list attr.inputs
      | JC { target = Cind { target }; _ } ->
          RegIdSet.union
            (gather_regs [ target ] |> RegIdSet.of_list)
            (RegIdSet.of_list
               (ASIR.REA.caller_saved_regs
               |> List.map (fun x -> RegId.Register x)))
      | JT { target = Cdirect { attr = attr2 }; attr } ->
          RegIdSet.union
            (RegIdSet.of_list attr2.inputs)
            (RegIdSet.of_list attr.returns)
      | JT { target = Cind { target }; attr } ->
          RegIdSet.union
            (gather_regs [ target ] @ attr.returns |> RegIdSet.of_list)
            (RegIdSet.of_list
               (ASIR.REA.caller_saved_regs
               |> List.map (fun x -> RegId.Register x)))
      | JR { attr } -> RegIdSet.of_list attr
      | JI (Jjump_ind v) -> gather_regs [ v.target ] |> RegIdSet.of_list
      | JI (Jcbranch { condition; _ }) ->
          gather_regs [ condition ] |> RegIdSet.of_list
      | JI _ -> RegIdSet.empty
    in
    RegIdSet.union a gen

  let compute_used_regs (a : RegIdSet.t) (b : Block.t) : RegIdSet.t =
    let a = compute_used_regs_jmp a b.jmp in
    List.fold_right
      (fun (i : Inst.t_full) a -> compute_used_regs_inst a i)
      b.body a

  let analyze ((bs, e, bf) : ICFG.G.E.t) (a : t) : t =
    match e with
    | ICFG.EdgeLabel.Inner ->
        compute_used_regs (Option.value a ~default:RegIdSet.empty) bs.block
        |> Option.some
    | ICFG.EdgeLabel.Flow -> a

  let bot = RegIdSet.empty
end

module RegAnalysis = Graph.Fixpoint.Make (ICFG.G) (RegAnalysisDomain)

let compute_ud_func_with_graph (f : Func.t) (g : ICFG.G.t) : RegIdSet.t LocMap.t
    =
  failwith "TODO"

(*let analyze_res =
    RegAnalysis.analyze
      (fun v -> None)
      g
  in
  List.fold_left
    (fun m (b : Block.t) ->
      let a = analyze_res { block = b; time = Pre } in
      List.fold_left
        (fun ((a, m) : RegIdSet.t LocMap.t) (i : Inst.t_full) ->
          let a, ns = compute_ud_inst a i in
          (a, LocMap.add i.loc ns m))
        (a, m) b.body
      |> fun (a, m) ->
      let _, ns = compute_ud_jmp a b.jmp in
      LocMap.add b.jmp.loc ns m)
    LocMap.empty f.blocks *)

let compute_ud_func (f : Func.t) : RegIdSet.t LocMap.t =
  compute_ud_func_with_graph f (ICFG.to_graph f.blocks)
