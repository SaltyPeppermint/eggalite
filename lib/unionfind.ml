module UnionFind : sig
  type t
  type id = int

  val create : unit -> t
  val make_set : t -> id
  val size : t -> int
  val find : t -> id -> id
  val union : t -> id -> id -> id
end = struct
  type id = int
  type t = { mutable parents : id option array; mutable size : int }

  let create () = { parents = Array.make 16 None; size = 0 }

  let resize (uf : t) (new_size : int) =
    let new_parents = Array.make new_size None in
    Array.blit uf.parents 0 new_parents 0 uf.size;
    uf.parents <- new_parents

  let make_set (uf : t) =
    let id = uf.size in
    if id >= Array.length uf.parents then resize uf (Array.length uf.parents * 2);
    uf.parents.(id) <- Some id;
    uf.size <- uf.size + 1;
    id

  let size uf = uf.size

  let get_parent (uf : t) (query : id) =
    if query < 0 || query >= uf.size then
      invalid_arg "UnionFind.parent: invalid id";
    uf.parents.(query) |> Option.get

  let set_parent (uf : t) (query : id) (new_parent : id) =
    if query < 0 || query >= uf.size then
      invalid_arg "UnionFind.parent_mut: invalid id";

    uf.parents.(query) <- Option.some new_parent

  let rec find uf current =
    let p = get_parent uf current in
    if p = current then current else find uf p

  let union uf root1 root2 =
    set_parent uf root2 root1;
    root1
end
