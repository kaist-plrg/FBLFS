open Common
open Syn
module ICFG = Common.ICFGF.Make (Block)

module LivenessAnalysisDomain = struct
  type t = RegIdSet.t
  type data = RegIdSet.t
  type edge = ICFG.G.E.t
  type vertex = ICFG.G.V.t
  type g = ICFG.G.t

  let join = RegIdSet.union
  let le = RegIdSet.subset
  let direction = Graph.Fixpoint.Backward
  let equal = RegIdSet.equal

  (* Analyze function computes live variables after each instruction *)
  let analyze (e : edge) (d : data) : data = failwith "Not implemented"
  (*
    let inst = ICFG.G.E.label e in
    let def_regs =
      match inst with
      | First (Load { output; _ }) -> RegIdSet.singleton output.id
      | First (Store _) -> RegIdSet.empty
      | Third { output; _ } -> RegIdSet.singleton output.id
      | _ -> RegIdSet.empty
    in
    let use_regs =
      match inst with
      | First (Load { pointer = Register rm; _ }) -> RegIdSet.singleton rm.id
      | First (Store { pointer = Register rm; value = Register rs; _ }) ->
          RegIdSet.of_list [ rm.id; rs.id ]
      | First (Store { pointer = Const _; value = Register rs; _ }) ->
          RegIdSet.singleton rs.id
      | Third { expr; _ } ->
          expr_uses expr (* You'll need to implement expr_uses *)
      | _ -> RegIdSet.empty
    in
    RegIdSet.union use_regs (RegIdSet.diff d def_regs)
    *)
end

module LivenessAnalysis = Graph.Fixpoint.Make (ICFG.G) (LivenessAnalysisDomain)

let remove_dead_assignments_func (f : Func.t) : Func.t =
  failwith "Not implemented"
(*
  let g = ICFG.to_graph f.body in
  let live_vars = LivenessAnalysis.analyze g in

  (* Filter out dead assignments *)
  let filter_dead_inst (inst : Inst.t) live_after =
    match inst with
    | First (Load { output; _ }) ->
        (* Keep the load if its output is used *)
        RegIdSet.mem output.id live_after
    | Third { output; _ } ->
        (* Keep the assignment if its output is used *)
        RegIdSet.mem output.id live_after
    | _ -> true (* Keep all other instructions *)
  in

  (* Create new blocks with dead assignments removed *)
  let new_blocks =
    List.map
      (fun block ->
        let live_at_end = live_vars (ICFG.G.V.create block) in
        let filtered_insts =
          List.filter
            (fun inst -> filter_dead_inst inst live_at_end)
            block.insts
        in
        { block with insts = filtered_insts })
      f.body
  in

  { f with body = new_blocks }
  *)

let remove_dead_assignments (p : Prog.t) : Prog.t =
  let funcs = Prog.funcs p in
  { p with funcs = List.map remove_dead_assignments_func funcs }
