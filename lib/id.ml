open! Core

module Id : sig
  type t

  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = Id of int

  let of_int x = Id x
  let to_int x = match x with Id i -> i
end
