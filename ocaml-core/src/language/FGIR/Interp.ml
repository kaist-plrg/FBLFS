open Common
open Syn
open Sem

let step_call_internal (s : State.t) (p : Prog.t)
    ({ target = { target = calln; attr }; attr = (); fallthrough } : SCall.t) :
    (Store.t * Stack.elem_t, StopEvent.t) Result.t =
  let* currf = State.get_current_function s p |> StopEvent.of_str_res in
  let* f = State.get_func_from p calln |> StopEvent.of_str_res in
  (s.sto, (s.cursor, fallthrough)) |> Result.ok

let step_ret (s : State.t) (p : Prog.t) ({ attr } : SRet.t) (calln, retn') :
    (Store.t, StopEvent.t) Result.t =
  let* attr = Value.try_loc attr |> StopEvent.of_str_res in
  if Loc.compare attr retn' <> 0 then
    StopEvent.FailStop "Retaddr not matched" |> Result.error
  else Ok s.sto

let action_JC = State.mk_action_JC step_call_internal
let action_JR = State.mk_action_JR step_ret

let step_ins (p : Prog.t) (ins : Inst.t) (s : Store.t) (curr : Cursor.t) :
    (StoreAction.t, String.t) Result.t =
  Inst.fold (Store.step_ILS s) (Store.step_IA s) (Store.step_IN s)
    (Store.step_SP World.Environment.x64_syscall_table s)
    ins

let step_jmp (p : Prog.t) (jmp : Jmp.t) (s : State.t) :
    (Action.t, String.t) Result.t =
  match jmp with
  | JI ji -> State.step_JI p s ji
  | JC jc -> (
      let* sc = SCall.eval s.sto jc in
      match Byte8Map.find_opt (Loc.get_addr sc.target.target) p.externs with
      | None -> Action.call sc |> Result.ok
      | Some name -> State.step_call_external p s.sto name sc.fallthrough)
  | JR jr ->
      let* sr = SRet.eval s.sto jr in
      Action.ret sr |> Result.ok
  | JT _ -> Error "unimplemented jump"

let step (p : Prog.t) (s : State.t) : (Action.t, StopEvent.t) Result.t =
  match s.cont with
  | { remaining = []; jmp } -> step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = { ins = Third _; _ } :: []; jmp } ->
      step_jmp p jmp.jmp s |> StopEvent.of_str_res
  | { remaining = i :: []; jmp = { jmp = JI (JIntraF.Jfallthrough l) } } ->
      let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
      Action.of_store a (Some l) |> Result.ok
  | { remaining = i :: res; jmp } ->
      if List.is_empty res then
        StopEvent.FailStop "Not possible inst" |> Result.error
      else
        let* a = step_ins p i.ins s.sto s.cursor |> StopEvent.of_str_res in
        Action.of_store a None |> Result.ok

let action_store (p : Prog.t) (sto : Store.t) (a : StoreAction.t) (s : State.t)
    : (Store.t, StopEvent.t) Result.t =
  match a with
  | Special ("syscall", sides, vals) ->
      let* syscall_num =
        match vals with
        | Interop.VArith (VInt (V64 n)) :: _ -> Ok n
        | _ -> Error "syscall: invalid syscall number" |> StopEvent.of_str_res
      in
      let* fsig =
        World.Environment.x64_syscall_table syscall_num
        |> Option.to_result
             ~none:(StopEvent.FailStop "syscall: invalid syscall number")
      in
      let* res =
        World.Environment.x64_do_syscall vals |> StopEvent.of_str_res
      in
      let* sto = Store.build_sides sto sides |> StopEvent.of_str_res in
      Store.build_ret sto res |> StopEvent.of_str_res
  | Special ("LOCK", _, _) | Special ("UNLOCK", _, _) -> sto |> Result.ok
  | Special (x, _, _) ->
      Error (Format.sprintf "unimplemented special %s" x)
      |> StopEvent.of_str_res
  | Assign (p, v) -> Store.action_assign sto p v |> StopEvent.of_str_res
  | Load (r, p, v) -> Store.action_load sto r p v |> StopEvent.of_str_res
  | Store (p, v) -> Store.action_store sto p v |> StopEvent.of_str_res
  | Nop -> Store.action_nop sto |> StopEvent.of_str_res

let action (p : Prog.t) (s : State.t) (a : Action.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = action_store p s.sto a s in
      match (lo, s.cont) with
      | None, { remaining = _ :: res; jmp } ->
          Ok { s with sto; cont = { remaining = res; jmp } }
      | Some l, _ ->
          let* cont =
            Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
          in
          Ok { s with sto; cont }
      | _ -> StopEvent.FailStop "Not possible inst" |> Result.error)
  | Jmp l -> State.action_jmp p s l |> StopEvent.of_str_res
  | ExternCall (name, sides, args, ft) -> (
      match World.Environment.request_call_opt name args with
      | Some retv -> State.action_extern p s sides retv ft
      | None -> StopEvent.NormalStop |> Result.error)
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let action_with_computed_syscall (p : Prog.t) (s : State.t)
    (sides : (Value.t * bytes) List.t) (res : Interop.t) (lo : Loc.t Option.t) :
    (State.t, StopEvent.t) Result.t =
  let* sto = Store.build_sides s.sto sides |> StopEvent.of_str_res in
  let* sto = Store.build_ret sto res |> StopEvent.of_str_res in
  match (lo, s.cont) with
  | None, { remaining = _ :: res; jmp } ->
      Ok { s with sto; cont = { remaining = res; jmp } }
  | Some l, _ ->
      let* cont =
        Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
      in
      Ok { s with sto; cont }
  | _ -> StopEvent.FailStop "Not possible inst" |> Result.error

let action_with_computed_extern (p : Prog.t) (s : State.t) (a : Action.t)
    (sides : ('a * Bytes.t) List.t) (retv : Interop.t) :
    (State.t, StopEvent.t) Result.t =
  match a with
  | StoreAction (a, lo) -> (
      let* sto = action_store p s.sto a s in
      match (lo, s.cont) with
      | None, { remaining = _ :: res; jmp } ->
          Ok { s with sto; cont = { remaining = res; jmp } }
      | Some l, _ ->
          let* cont =
            Cont.of_loc p (State.get_func_loc s) l |> StopEvent.of_str_res
          in
          Ok { s with sto; cont }
      | _ -> StopEvent.FailStop "Not possible inst" |> Result.error)
  | Jmp l -> State.action_jmp p s l |> StopEvent.of_str_res
  | ExternCall (name, sidesn, args, ft) ->
      if List.length sides = List.length sidesn then
        let fake_sides =
          List.map
            (fun ((ptr, _), (_, bytes)) -> (ptr, bytes))
            (List.combine sidesn sides)
        in
        State.action_extern p s fake_sides retv ft
      else Error "Not same call target" |> StopEvent.of_str_res
  | Call sc -> action_JC p s sc
  | TailCall st -> StopEvent.FailStop "unimplemented jump" |> Result.error
  | Ret sr -> action_JR p s sr

let rec interp (p : Prog.t) (s : State.t) : (State.t, StopEvent.t) Result.t =
  if Loc.equal (State.get_cont s |> Cont.get_loc) (Loc.of_addr_seq (0x0L, 0))
  then [%log info "interp: %a" State.pp s];

  let* a =
    step p s |> Fun.flip StopEvent.add_loc (Cont.get_loc (State.get_cont s))
  in
  let* s' =
    action p s a |> Fun.flip StopEvent.add_loc (Cont.get_loc (State.get_cont s))
  in
  interp p s'
