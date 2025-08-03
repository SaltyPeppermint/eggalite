module type HashCons = sig
  type t
  type 'a hashcons

  val create : unit -> t hashcons
  val intern : t hashcons -> t -> int
  val get : t hashcons -> int -> t option
  val length : t hashcons -> int
  val is_empty : t hashcons -> bool
end

module MakeHashCons (H : Hashtbl.HashedType) : HashCons with type t = H.t =
struct
  module HT = Hashtbl.Make (H)

  type t = H.t

  type 'a hashcons = {
    mutable values : 'a option array;
    indices : int HT.t;
    mutable size : int;
  }

  let create () =
    {
      values = Array.make 16 None;
      (* Initial size, will grow as needed *)
      indices = HT.create 16;
      size = 0;
    }

  let resize (hc : 'a hashcons) (new_size : int) =
    let new_values = Array.make new_size None in
    Array.blit hc.values 0 new_values 0 hc.size;
    hc.values <- new_values

  let intern (hc : 'a hashcons) (v : 'a) =
    match HT.find_opt hc.indices v with
    | Some idx -> idx
    | None ->
        let idx = hc.size in
        if idx >= Array.length hc.values then
          resize hc (Array.length hc.values * 2);
        hc.values.(idx) <- Some v;
        HT.add hc.indices v idx;
        hc.size <- hc.size + 1;
        idx

  let get (hc : 'a hashcons) (idx : int) =
    if idx >= 0 && idx < hc.size then
      let i = hc.values.(idx) in
      if Option.is_none i then invalid_arg "HashCons.get: invalid index" else i
    else None

  let length (hc : 'a hashcons) = hc.size
  let is_empty (hc : 'a hashcons) = hc.size = 0
end
