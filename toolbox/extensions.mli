module Array: sig
    include module type of Array
    val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b array -> 'a
end

module List: sig
    include module type of List
    val replicate: int -> 'a -> 'a list
    val reject: ('a -> bool) -> 'a list -> 'a list
    val equal: ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
    val map_pairs: ('a -> 'a -> 'b) -> 'a list -> 'b list
    val flist: (('a -> 'b) list) -> 'a -> 'b list
    val take: int -> 'a list -> 'a list
    val drop: int -> 'a list -> 'a list
    val product: 'a list -> 'b list -> ('a * 'b) list
    val populate: ('a -> bool) -> ('a -> 'a) -> 'a -> 'a list
    val zip_with: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val zip: 'a list -> 'b list -> ('a * 'b) list

    val sum: int list -> int
    val prod: int list -> int
end

module Seq: sig
    include module type of Seq
    val take: int -> 'a Seq.t -> 'a Seq.t
    val concat: 'a Seq.t Seq.t -> 'a Seq.t
    val repeat: 'a -> 'a Seq.t
    val populate: ('a -> bool) -> ('a -> 'a) -> 'a -> 'a Seq.t
    val zip_with: ('a -> 'b -> 'c) -> 'a Seq.t -> 'b Seq.t -> 'c Seq.t
    val zip: 'a Seq.t -> 'b Seq.t -> ('a * 'b) Seq.t
    val find_opt: ('a -> bool) -> 'a Seq.t -> 'a option
    val resident: 'a t -> 'a t
end

module String: sig
    include module type of String
    val to_chars: String.t -> char list
end
