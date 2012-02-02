(* NOTE: this version of number.ml differs from that in base, with the
   0/0 as described below
*)

module type NUMBER =
sig
  type t
  val zero: t
  val one: t
  val of_int: int -> t
  val to_int: t -> int
  val of_float: float -> t
  val succ: t -> t
  val neg: t -> t
  val abs: t -> t
  val inv: t -> t
  val exp: t -> t
  val log: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val max: t -> t -> t
  val pow: t -> t -> t
  val indic: t -> t -> t (* indicator function *)
  val compare: t -> t -> int
  val to_string: t -> string
  val of_string: string -> t
end


module B =
struct
  type t = bool
  let of_int x = (x mod 2) = 1
  let to_int x = if x then 1 else 0
  let of_float x = of_int (int_of_float x)
  let to_float x = float_of_int (to_int x)
  let neg x = x
  let add x y = if x && y then false else x || y
  let sub = add
  let mul = ( && )
  let div x y = false (* the fun stops here *)
  let max = ( || )
  let pow x y = if y then x else true
  let indic = ( = )
  let compare = Pervasives.compare
  let succ = not
  let abs x = x
  let inv x = false
  let exp x= true
  let log x = false
  let zero = false
  let one = true
  let to_string x = if x then "1" else "0"
  let of_string s = of_int (int_of_string s)
end

(* NOTE: we define 0/0 = 0 *)
module Z =
struct
  type t = int
  let of_int x = x
  let to_int x = x
  let of_float x = int_of_float x
  let neg x = -x
  let add = (+)
  let sub = (-)
  let mul = ( * )
  let div x y = if x = 0 then 0 else x/y
  let max = max
  let pow x y =
    if y = 0 then 1
    else if y < 0 then 0
    else
      let rec aux n =
	if n = 0 then 1
	else x * (aux (n-1))
      in
      aux y
  let indic x y =
    if x = y then 1 else 0
  let compare = Pervasives.compare
  let succ = (+) 1
  let abs = abs
  let conv_float_fun f =
    (fun x -> int_of_float (f (float_of_int x) ))
  let inv = conv_float_fun (fun x -> 1./.x)
  let exp = conv_float_fun exp
  let log = conv_float_fun log
  let zero = 0
  let one = 1
  let to_string x = Printf.sprintf "%d" x
  let of_string = int_of_string
end


(* below: a bit of care needs to be taken in order to have infinites and nan not
 * get converted into ok numbers by indic or div.
 * *)
let is_bad x = let c = classify_float x in (c = FP_infinite) || (c = FP_nan)
let is_ok x = not (is_bad x)
let ucheck f x = if is_bad x then nan else f x
let bcheck f x y = if is_bad x || is_bad y then nan else f x y

(* NOTE: we define 0/0 = 0 *)
module R =
struct
  type t = float
  let of_int x = float_of_int x
  let to_int x = int_of_float x
  let of_float x = x
  let neg x = -.x
  let add = (+.)
  let sub = (-.)
  let mul = ( *. )
  let div = bcheck (fun x y -> if x = 0. then 0. else x/.y)
  let max = bcheck max
  let pow = bcheck ( ** )
  let indic = bcheck (fun x y -> if x = y then 1. else 0.)
  let compare = Pervasives.compare
  let succ = (+.) 1.
  let abs = abs_float
  let inv = ucheck (fun x -> 1. /. x)
  let exp = ucheck exp
  let log = ucheck log
  let zero = 0.
  let one = 1.
  let to_string x = Printf.sprintf "%g" x
  let of_string = float_of_string
end

module Z64 =
struct
  type t = Int64.t
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let of_float = Int64.of_float
  let neg = Int64.neg
  let add = Int64.add
  let sub = Int64.sub
  let mul = Int64.mul
  let div = Int64.div
  let max a b = if (Int64.compare a b) > 0 then a else b
  let pow x y =
    if y  = Int64.zero then Int64.one
    else if (Int64.compare y Int64.zero) < 0 then Int64.zero
    else
      let rec aux n =
        if n = Int64.zero then Int64.one
        else Int64.mul x (aux (Int64.pred n))
      in
      aux y
  let indic x y =
    if x = y then Int64.one else Int64.zero
  let compare = Pervasives.compare
  let succ = Int64.succ
  let abs = Int64.abs
  let conv_float_fun f =
    (fun x -> Int64.of_float (f (Int64.to_float x) ))
  let inv = conv_float_fun (fun x -> 1./.x)
  let exp = conv_float_fun exp
  let log = conv_float_fun log
  let zero = Int64.zero
  let one = Int64.one
  let to_string = Int64.to_string
  let of_string = Int64.of_string
end
