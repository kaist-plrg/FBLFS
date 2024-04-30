open StdlibExt
open SleighDef
open World
open Notation

let usage_msg = "slaparser -i <ifile>"
let ifile = ref ""
let test_dir = ref ""
let inputs : String.t List.t ref = ref []

let speclist =
  [
    ("-i", Arg.Set_string ifile, ": input file");
    ("-t", Arg.Set_string test_dir, ": test spec directory");
  ]

let print_tag (xml : Xml.xml) : Unit.t = [%log info "tag: %s" (Xml.tag xml)]

let check (b : Bool.t) : (Unit.t, String.t) Result.t =
  if b then Ok () else Error ""

let print_attribs (xml : Xml.xml) : Unit.t =
  Xml.attribs xml |> List.iter (fun (n, s) -> [%log info "%s: %s" n s])

module StringSet = Set.Make (String)

let do_a (s : SleighDef.Sla.t) (input : String.t) : Unit.t =
  let res =
    [%log debug "scopes: %d" (Int32Map.cardinal s.symbol_table.scopeMap)];
    [%log debug "symbols: %d" (Int32Map.cardinal s.symbol_table.symbolMap)];
    [%log
      debug "instruction constructor length: %d"
        (ConstructorMap.cardinal s.root.construct)];
    let pw = ParserWalker.of_mock input in
    let* v = Sla.resolve s s.root pw in
    let* s = SymbolPrinter.print_constructor v s pw in
    [%log info "resolve: %s" s] |> Result.ok
  in
  match res with Ok () -> () | Error s -> [%log info "%s" s]

let do_single_file (fname : String.t) (inputs : String.t List.t) : Unit.t =
  inputs
  |> List.iter (fun input ->
         let s =
           let* xmlf =
             try Xml.parse_file fname |> Result.ok
             with Xml_light_errors.Xml_error s -> Xml.error s |> Result.error
           in
           Sla.decode xmlf
         in
         match s with Ok s -> do_a s input | Error s -> [%log info "%s" s])

let do_test_dir (dname : String.t) : Unit.t =
  let processor_dir = dname ^ "/Ghidra/Processors" in
  let dirs = Sys.readdir processor_dir |> Array.to_list in
  let dirs = List.filter (fun d -> d.[0] <> '.') dirs in
  let dirs =
    List.map (fun d -> processor_dir ^ "/" ^ d ^ "/data/languages") dirs
  in
  let testspecs =
    List.map
      (fun d ->
        (try Sys.readdir d |> Array.to_list with Sys_error _ -> [])
        |> List.filter_map (fun f ->
               if Filename.check_suffix f ".sla" then Some (d ^ "/" ^ f)
               else None))
      dirs
    |> List.concat
  in
  List.iter
    (fun f ->
      let s =
        let* xmlf =
          try Xml.parse_file f |> Result.ok
          with Xml_light_errors.Xml_error s -> Xml.error s |> Result.error
        in
        Sla.decode xmlf
      in
      match s with
      | Ok s -> [%log info "loaded %s" f]
      | Error s -> [%log info "failed to load %s" f])
    testspecs

let hex2str (v : String.t) : String.t =
  let len = String.length v in
  let buf = Buffer.create (len / 2) in
  for i = 0 to (len / 2) - 1 do
    let c =
      Char.chr (Scanf.sscanf (String.sub v (i * 2) 2) "%x" (fun x -> x))
    in
    Buffer.add_char buf c
  done;
  Buffer.contents buf

let main () =
  let ghidra_path = [%pwd] ^ "/ghidra_11.0.3_PUBLIC" in
  let processor =
    ghidra_path ^ "/Ghidra/Processors/x86/data/languages/x86-64.sla"
  in
  Arg.parse speclist (fun x -> inputs := hex2str x :: !inputs) usage_msg;
  inputs := List.rev !inputs;
  match (!ifile, !test_dir) with
  | "", "" -> do_single_file processor !inputs
  | fname, "" -> do_single_file fname !inputs
  | "", dname -> do_test_dir dname
  | _ -> raise (Arg.Bad "Cannot specify both input file and test directory")

let () = Global.run_main main
