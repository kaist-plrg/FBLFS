open Common
open Syn

let is_equal_sload (i : Inst.t) (j : Inst.t) : Bool.t =
  match (i, j) with
  | ( Second (Sload { output = o1; offset = p1 }),
      Second (Sload { output = o2; offset = p2 }) ) ->
      RegId.compare_full o1 o2 = 0 && Int64.equal p1 p2
  | _ -> false

let translate_func (f : Func.t) : Func.t =
  (* Create new blocks with dead assignments removed *)
  let new_blocks =
    List.map
      (fun (block : Block.t) ->
        let body =
          List.fold_right
            (fun (inst : Inst.t_full) (bodies : Inst.t_full List.t) ->
              match bodies with
              | [] -> [ inst ]
              | hd :: tl ->
                  if is_equal_sload inst.ins hd.ins then bodies
                  else inst :: bodies)
            block.body []
        in
        { block with body })
      f.blocks
  in

  { f with blocks = new_blocks }

let translate_prog (p : Prog.t) : Prog.t =
  let funcs = Prog.funcs p in
  { p with funcs = List.map translate_func funcs }
