open Common
open Basic_domain
open Value_domain
open Syn

type __ = {
  value_nonrel : NonRelStateD.t;
  value_octagon : OctagonD.t;
  value_boolpower : BoolPowerD.t;
}

module Lattice_noBot = struct
  include
    TripleD.MakeJoinSemiLatticeWithTop_Record (NonRelStateD) (OctagonD)
      (BoolPowerD)
      (struct
        type t = __

        let get_fst a = a.value_nonrel
        let get_snd a = a.value_octagon
        let get_trd a = a.value_boolpower

        let make a b c =
          { value_nonrel = a; value_octagon = b; value_boolpower = c }
      end)

  let equal (a : t) (b : t) =
    NonRelStateD.le a.value_nonrel b.value_nonrel
    && NonRelStateD.le b.value_nonrel a.value_nonrel

  let pp fmt (a : t) =
    Format.fprintf fmt "NonRelState: %a\nOctagon: %a\nBoolPower: %a\n"
      NonRelStateD.pp a.value_nonrel OctagonD.pp a.value_octagon BoolPowerD.pp
      a.value_boolpower

  let gen_aexpr_set (o : OctagonD.t) (v : VarNode.t) : AExprSet.t =
    match v with
    | Register r -> OctagonD.find_all_equiv o r
    | _ -> AExprSet.empty

  let clear_memref (a : t) : t =
    {
      value_nonrel = NonRelStateD.clear_memref a.value_nonrel;
      value_octagon = OctagonD.clear_memref a.value_octagon;
      value_boolpower = BoolPowerD.clear_memref a.value_boolpower;
    }

  let clear_tempreg (a : t) : t = a
  (*
    {
      value_nonrel = NonRelStateD.clear_tempreg a.value_nonrel;
      value_octagon = OctagonD.clear_tempreg (OctagonD.floyd_warshall a.value_octagon);
      value_boolpower = BoolPowerD.clear_tempreg a.value_boolpower;
    } *)

  let post_single (rom : DMem.t) (ls : Loc.t) (a : t) (i : Inst.t) : t =
    match i with
    | Second { expr; output } ->
        {
          value_nonrel =
            NonRelStateD.process_assignment a.value_nonrel a.value_octagon expr
              output;
          value_octagon =
            OctagonD.process_assignment a.value_octagon expr output;
          value_boolpower =
            BoolPowerD.process_assignment a.value_boolpower a.value_octagon expr
              output;
        }
    | First (Load { pointer; output; _ }) ->
        let addrSet = gen_aexpr_set a.value_octagon pointer in
        let regSet = AExprSet.used_regs addrSet in
        let inter_regSet =
          RegIdFullSet.inter regSet (OctagonD.memory_base_regs a.value_octagon)
        in
        let inter_addrSet =
          AExprSet.filter
            (fun { base; offset } -> RegIdFullSet.mem base inter_regSet)
            addrSet
        in
        let a, addrSet =
          if AExprSet.is_empty inter_addrSet then (clear_memref a, addrSet)
          else (a, inter_addrSet)
        in

        {
          value_nonrel =
            NonRelStateD.process_load rom a.value_nonrel a.value_octagon output
              addrSet;
          value_octagon =
            OctagonD.process_load rom a.value_octagon output addrSet;
          value_boolpower =
            BoolPowerD.process_load rom a.value_boolpower a.value_octagon output
              addrSet;
        }
    | First (Store { pointer; value; _ }) ->
        let addrSet = gen_aexpr_set a.value_octagon pointer in
        let regSet = AExprSet.used_regs addrSet in
        let inter_regSet =
          RegIdFullSet.inter regSet (OctagonD.memory_base_regs a.value_octagon)
        in
        let inter_addrSet =
          AExprSet.filter
            (fun { base; offset } -> RegIdFullSet.mem base inter_regSet)
            addrSet
        in
        let a, addrSet =
          if AExprSet.is_empty inter_addrSet then (clear_memref a, addrSet)
          else (a, inter_addrSet)
        in
        {
          value_nonrel =
            NonRelStateD.process_store a.value_nonrel a.value_octagon value
              addrSet;
          value_octagon = OctagonD.process_store a.value_octagon value addrSet;
          value_boolpower =
            BoolPowerD.process_store a.value_boolpower a.value_octagon value
              addrSet;
        }
    | Third _ -> a
    | Fourth _ -> a (* TODO: handle special instruction *)

  let filter_branch (a : t) (condv : VarNode.t) (trueloc : Loc.t)
      (targetloc : Loc.t) : t =
    match condv with
    | Register r -> (
        match BoolPowerD.find_opt (KReg r) a.value_boolpower with
        | Some b ->
            if compare trueloc targetloc = 0 then
              {
                value_nonrel = a.value_nonrel;
                value_octagon = fst b;
                value_boolpower = a.value_boolpower;
              }
            else
              {
                value_nonrel = a.value_nonrel;
                value_octagon = snd b;
                value_boolpower = a.value_boolpower;
              }
        | _ -> a)
    | _ -> a

  let try_concretize_vn (a : t) (vn : VarNode.t) : Int64Set.t option =
    match vn with
    | Register u ->
        OctagonD.find_all_equiv a.value_octagon u
        |> AExprSet.to_list
        |> List.fold_left
             (fun acc (ae : AExpr.t) ->
               match acc with
               | Some _ -> acc
               | None -> (
                   match
                     NonRelStateD.find_opt (KReg ae.base) a.value_nonrel
                   with
                   | Some a ->
                       AbsNumeric.try_concretize a
                       |> Option.map (fun x ->
                              Int64Set.map
                                (fun x -> Int64.add x (Z.to_int64 ae.offset))
                                x)
                   | None -> None))
             None
    | _ -> None
end

type t = LV of (Lattice_noBot.t * DMem.t) | Bottom
type edge = ICFG.G.E.t

let join a b =
  match (a, b) with
  | LV a, LV b -> LV (Lattice_noBot.join (fst a) (fst b), snd a)
  | Bottom, _ -> b
  | _, Bottom -> a

let widening a b =
  match (a, b) with
  | LV a, LV b -> LV (Lattice_noBot.top, snd a)
  | Bottom, _ -> b
  | _, Bottom -> a

let pp fmt (a : t) =
  match a with
  | LV a -> Lattice_noBot.pp fmt (fst a)
  | Bottom -> Format.fprintf fmt "Bottom"

let bot = Bottom
let init (rom : DMem.t) = LV (Lattice_noBot.top, rom)

let equal a b =
  match (a, b) with
  | LV a, LV b -> Lattice_noBot.equal (fst a) (fst b)
  | Bottom, Bottom -> true
  | _ -> false

let analyze_noBot (e : edge) (a : Lattice_noBot.t) (rom : DMem.t) :
    Lattice_noBot.t =
  match ICFG.G.E.label e with
  | Inner ->
      [%log debug "Analyzing edge %a" Loc.pp (ICFG.G.E.src e).block.loc];
      List.fold_left
        (fun a (i : Inst.t_full) ->
          let a2 =
            if Loc.get_seq i.loc = 0 then Lattice_noBot.clear_tempreg a else a
          in
          let value = Lattice_noBot.post_single rom i.loc a2 i.ins in
          (* if Int64.compare (Loc.get_addr i.loc) 0x0L = 0 then
             [%log
               info "Value after instruction %a: %a" Loc.pp i.loc
                 Lattice_noBot.pp value]; *)
          value)
        a (ICFG.G.E.src e).block.body
  | Flow -> (
      let srcBlock = ICFG.G.E.src e in
      match srcBlock.block.jmp.jmp with
      | JI (Jcbranch { condition; target_true; target_false }) ->
          (* if OctagonD.check_neg_cycle a.value_octagon then
             [%log fatal "Negative cycle detected BEFORE"]; *)
          let value =
            Lattice_noBot.filter_branch a condition target_true
              (ICFG.G.E.dst e).block.loc
          in
          value
      | _ -> a)

let analyze (e : edge) (a : t) : t =
  match a with
  | LV a -> LV (analyze_noBot e (fst a) (snd a), snd a)
  | Bottom -> Bottom
