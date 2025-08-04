open! Core

module type HashCons = sig
  type t
  type hashcons

  val create : unit -> hashcons
  val intern : hashcons -> t -> int
  val get : hashcons -> int -> t option
  val length : hashcons -> int
  val is_empty : hashcons -> bool
end

module MakeHashCons (H : Hashtbl.Key) : HashCons with type t = H.t = struct
  module HT = Hashtbl.Make (H)

  type t = H.t

  type hashcons = {
    mutable values : t option array;
    indices : int HT.t;
    mutable size : int;
  }

  let create () =
    {
      values = Array.create ~len:16 None;
      (* Initial size, will grow as needed *)
      indices = HT.create ~growth_allowed:true ~size:16 ();
      size = 0;
    }

  let resize (hc : hashcons) (new_size : int) =
    let new_values = Array.create ~len:new_size None in
    Array.blit ~src:hc.values ~src_pos:0 ~dst:new_values ~dst_pos:0 ~len:hc.size;
    hc.values <- new_values

  let intern (hc : hashcons) (v : 'a) =
    match Hashtbl.find hc.indices v with
    | Some idx -> idx
    | None ->
        let idx = hc.size in
        if idx >= Array.length hc.values then
          resize hc (Array.length hc.values * 2);
        hc.values.(idx) <- Some v;
        Hashtbl.add hc.indices ~key:v ~data:idx |> ignore;
        hc.size <- hc.size + 1;
        idx

  let get (hc : hashcons) (idx : int) =
    if idx >= 0 && idx < hc.size then
      let i = hc.values.(idx) in
      if Option.is_none i then invalid_arg "HashCons.get: invalid index" else i
    else None

  let length (hc : hashcons) = hc.size
  let is_empty (hc : hashcons) = hc.size = 0
end
