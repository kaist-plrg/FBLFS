open Common
open Syn
module ICFG = Liveness.ICFG

let translate_func (f : Func.t) : Func.t =
  let g = ICFG.to_graph f.blocks in
  let live_vars = Liveness.LivenessAnalysis.analyze (fun x -> None) g in

  (* Filter out dead assignments *)
  let filter_dead_inst (inst : Inst.t) live_after =
    match inst with
    | Third { output; _ } ->
        (* Keep the assignment if its output is used *)
        RegIdSet.mem output.id live_after
    | _ -> true (* Keep all other instructions *)
  in

  (* Create new blocks with dead assignments removed *)
  let new_blocks =
    List.map
      (fun block ->
        let live_at_end = live_vars { block; time = Post } in
        let body =
          match live_at_end with
          | None -> block.body
          | Some live_at_end ->
              Liveness.LivenessAnalysisDomain.compute_used_regs_fold_snd
                live_at_end block [] (fun s inst bodies ->
                  if filter_dead_inst inst.ins s then inst :: bodies else bodies)
        in
        { block with body })
      f.blocks
  in

  { f with blocks = new_blocks }

let translate_prog (p : Prog.t) : Prog.t =
  let funcs = Prog.funcs p in
  { p with funcs = List.map translate_func funcs }
