module Inner = struct
  type t = Unit.t
  type resolved_t = Unit.t

  let pp fmt (p : t) = Format.fprintf fmt ""
end

include Common.JRetF.Make (Inner)
