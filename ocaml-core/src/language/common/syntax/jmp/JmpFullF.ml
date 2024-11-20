type 'jmp_t poly_t_full = { jmp : 'jmp_t; loc : Loc.t; mnem : Mnemonic.t }
[@@deriving sexp]

module type S = sig
  type t
  type t_full
  type itarget_t

  val pp : Format.formatter -> t -> unit
  val pp_full : Format.formatter -> t_full -> unit
  val succ : t -> itarget_t List.t
  val succ_full : t_full -> itarget_t List.t
  val is_ret : t -> bool
  val is_ret_full : t_full -> bool
  val resolve_calltarget_opt : t -> Loc.t option
  val get_call_target_full : t_full -> Loc.t option
  val get_loc : t_full -> Loc.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_full_of_sexp : Sexplib.Sexp.t -> t_full
  val sexp_of_t_full : t_full -> Sexplib.Sexp.t
end

module type JumpSig = sig
  type t

  val pp : Format.formatter -> t -> unit
  val succ : t -> Loc.t List.t
  val is_ret : t -> bool
  val resolve_calltarget_opt : t -> Loc.t option
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
end

module Make (Jmp : JumpSig) = struct
  type t_full = Jmp.t poly_t_full [@@deriving sexp]
  type itarget_t = Loc.t

  let pp_full (fmt : Format.formatter) (a : t_full) =
    Format.fprintf fmt "%a: %a [%a]" Loc.pp a.loc Jmp.pp a.jmp Mnemonic.pp
      a.mnem

  let succ_full (jmp : t_full) = Jmp.succ jmp.jmp
  let is_ret_full (jmp : t_full) = Jmp.is_ret jmp.jmp
  let get_call_target_full (jmp : t_full) = Jmp.resolve_calltarget_opt jmp.jmp
  let get_loc (p : t_full) = p.loc
end

module MakeFromJmps
    (JIntra : JumpSig)
    (JCall : JumpSig)
    (JTailCall : JumpSig)
    (JRet : JumpSig) =
struct
  module Inner = struct
    type t = JI of JIntra.t | JC of JCall.t | JT of JTailCall.t | JR of JRet.t
    [@@deriving sexp]

    let pp fmt (a : t) =
      match a with
      | JI j -> JIntra.pp fmt j
      | JC j -> JCall.pp fmt j
      | JT j -> JTailCall.pp fmt j
      | JR j -> JRet.pp fmt j

    let succ jmp =
      match jmp with
      | JI j -> JIntra.succ j
      | JC j -> JCall.succ j
      | JT j -> JTailCall.succ j
      | JR j -> JRet.succ j

    let is_ret jmp =
      match jmp with
      | JI j -> JIntra.is_ret j
      | JC j -> JCall.is_ret j
      | JT j -> JTailCall.is_ret j
      | JR j -> JRet.is_ret j

    let resolve_calltarget_opt (j : t) : Loc.t option =
      match j with
      | JI j -> JIntra.resolve_calltarget_opt j
      | JC j -> JCall.resolve_calltarget_opt j
      | JT j -> JTailCall.resolve_calltarget_opt j
      | JR j -> JRet.resolve_calltarget_opt j
  end

  include Inner
  include Make (Inner)
end
