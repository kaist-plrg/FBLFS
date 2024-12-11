open Common

let translate_inst (i : IOIR.Syn.Inst.t_full) : Stmt.t =
  match i.ins with
  | First i -> LoadStore i
  | Second i -> SLoadStore i
  | Third i -> Assignment i
  | Fourth i -> Nop i
  | Fifth i -> Special i

let translate_call (c : IOIR.Syn.JCall.t) : Stmt.t =
  Call { target = c.target; attr = c.attr }

let translate_tailcall (c : IOIR.Syn.JTailCall.t) : Stmt.t =
  TailCall { target = c.target; attr = c.attr }

let translate_ret (r : IOIR.Syn.JRet.t) : Stmt.t = Ret { attr = r.attr }

let translate_stmts (sts : IOIR.Syn.Inst.t_full List.t)
    (appends : Stmt.t List.t) (l : JLabel.t Option.t) : Node.t =
  let stmts = List.map translate_inst sts in
  let stmts = stmts @ appends in
  match l with
  | None -> Node.StmtN { stmts; next = None }
  | Some l -> Node.StmtN { stmts; next = Some l }

let translate_jmp (j : IOIR.Syn.JIntra.t) (loc : Loc.t) (mnem : String.t) :
    Node.t =
  match j with
  | Jjump l ->
      Node.JmpN (JIntra.Jjump (JLabel.Concrete { jmp = false; loc = l }))
  | Jjump_ind { target; candidates; _ } ->
      Node.JmpN
        (JIntra.Jjump_ind
           {
             target;
             candidates =
               candidates |> LocSet.to_list
               |> List.map (fun x ->
                      (Loc.get_addr x, JLabel.Concrete { jmp = false; loc = x }));
           })
  | Jcbranch { condition; target_true; target_false } ->
      Node.JmpN
        (JIntra.Jcbranch
           {
             condition;
             target_true = JLabel.Concrete { jmp = false; loc = target_true };
             target_false = JLabel.Concrete { jmp = false; loc = target_false };
           })
  | Jfallthrough i ->
      Node.JmpN (JIntra.Jjump (JLabel.Concrete { jmp = false; loc = i }))
  | Junimplemented -> Node.JmpN JIntra.Junimplemented

let translate_block (b : IOIR.Syn.Block.t) : (JLabel.t * Node.t) List.t =
  let addr_stmts = JLabel.Concrete { jmp = false; loc = b.loc } in
  let addr_jmp = JLabel.Concrete { jmp = true; loc = b.jmp.loc } in
  match b.jmp.jmp with
  | JI ji ->
      [
        (addr_stmts, translate_stmts b.body [] (Some addr_jmp));
        (addr_jmp, translate_jmp ji b.jmp.loc b.jmp.mnem);
      ]
  | JC c ->
      [
        (addr_stmts, translate_stmts b.body [ translate_call c ] (Some addr_jmp));
        ( addr_jmp,
          translate_jmp (Common.JIntraF.Jjump c.fallthrough) b.jmp.loc
            b.jmp.mnem );
      ]
  | JT c ->
      [ (addr_stmts, translate_stmts b.body [ translate_tailcall c ] None) ]
  | JR r -> [ (addr_stmts, translate_stmts b.body [ translate_ret r ] None) ]

let translate_func_to_stmt (f : IOIR.Syn.Func.t) : Stmt.t =
  [%log debug "Translating function %a" (Option.pp String.pp) f.nameo];
  let init : Graph.t =
    {
      entry = JLabel.Concrete { jmp = false; loc = f.entry };
      body =
        List.map (fun (b : IOIR.Syn.Block.t) -> translate_block b) f.blocks
        |> List.flatten |> JLabelMap.of_list;
    }
  in
  Restructure.restruct init |> Restructure.reduce_dag_target
  |> Restructure.remove_out |> Restructure.remove_dag |> Restructure.to_stmt

let translate_func (f : IOIR.Syn.Func.t) : Func.t =
  let g = translate_func_to_stmt f in
  { nameo = f.nameo; entry = f.entry; body = g; attr = f.attr }

let translate_prog (p : IOIR.Syn.Prog.t) : Prog.t =
  let p =
    p |> IOIR.DAE.translate_prog |> IOIR.DuplicateSLoadRemove.translate_prog
  in
  {
    sp_num = p.sp_num;
    fp_num = p.fp_num;
    funcs = List.map (fun (f : IOIR.Syn.Func.t) -> translate_func f) p.funcs;
    rom = p.rom;
    rspec = p.rspec;
    externs = p.externs;
    objects = p.objects;
  }
