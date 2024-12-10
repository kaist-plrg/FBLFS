open Common
open Basic_domain
open Value_domain
open World

let usage_msg = "dafnyprinter <ifile>"
let ifile = ref ""
let cwd = ref ""
let args = ref []
let argv0 = ref ""

let speclist =
  [
    ("-log-path", Arg.String (fun x -> Logger.set_log_file x), ": log path");
    ( "-log-feature",
      Arg.String (fun x -> Logger.add_log_feature x),
      ": add log feature" );
  ]

let pp_dict (pp_k : Format.formatter -> 'a -> unit)
    (pp_v : Format.formatter -> 'b -> unit) (fmt : Format.formatter)
    (d : ('a * 'b) List.t) : unit =
  Format.fprintf fmt "{";
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
       (fun fmt (k, v) -> Format.fprintf fmt "%a: %a" pp_k k pp_v v))
    d;
  Format.fprintf fmt "}"

let pp_list (pp_v : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
    (l : 'a List.t) : unit =
  Format.fprintf fmt "[";
  Format.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_v)
    l;
  Format.fprintf fmt "]"

let pp_int64 (fmt : Format.formatter) (i : Int64.t) : unit =
  Format.fprintf fmt "%Ld" i

let print_dafny (dafny : SIOIR.Prog.t) (path : String.t) : Unit.t =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@.%!"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       SIOIR.DafnyPrinter.print_dafny_func)
    dafny.funcs;
  close_out oc

let main () =
  Arg.parse speclist
    (fun x ->
      if !ifile = "" then ifile := x else raise (Arg.Bad "too many input files"))
    usage_msg;
  if !argv0 = "" then argv0 := !ifile;
  args := !argv0 :: !args;
  if !ifile = "" then raise (Arg.Bad "No input file")
  else
    let data = Artifact.Loader.load !ifile in
    let dafny =
      match data with
      | Artifact.Data.L3 l3 -> SIOIR.Translate.translate_prog l3
      | _ -> failwith "Unsupported artifact"
    in
    let ifile_base = Filename.basename !ifile |> Filename.remove_extension in
    let ifile_path = Filename.dirname !ifile in
    print_dafny dafny (Filename.concat ifile_path (ifile_base ^ ".dafny"))

let () = Global.run_main main
