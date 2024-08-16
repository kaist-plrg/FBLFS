open Common
open Syn
open Sem

let from_signature (p : Prog.t) (args : String.t List.t) (a : Byte8.t) : State.t
    =
  let init_sp =
    { SPVal.func = Loc.of_addr a; timestamp = 0L; multiplier = 1L; offset = 0L }
  in
  let f = Prog.get_func_opt p (Loc.of_addr a) |> Option.get in
  {
    timestamp = 0L;
    sto =
      Store.init_from_sig p.rom p.rspec
        (Loc.of_addr a, 0L)
        (Frame.empty (fst f.attr.sp_boundary)
           (Int64.add (snd f.attr.sp_boundary) 4096L))
        (Value.sp init_sp) args;
    (*
       {
         regs =
           RegFile.add_reg (RegFile.empty p.rspec)
             { id = RegId.Register 32l; offset = 0l; width = 8l }
             (Value.sp init_sp);
         mem =
           Memory.of_global_memory (GlobalMemory.from_rom p.rom)
           |> Memory.add_local_frame
                ;
       }; *)
    cursor = { func = Loc.of_addr a; tick = 0L };
    cont = Cont.of_func_entry_loc p (Loc.of_addr a) |> Result.get_ok;
    stack = [];
  }

let default (p : Prog.t) (args : String.t List.t) : State.t =
  (List.find
     (fun (x : Func.t) -> Option.equal String.equal x.nameo (Some "main"))
     p.funcs)
    .entry |> Loc.get_addr |> from_signature p args
