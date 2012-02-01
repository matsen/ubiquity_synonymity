(* to represent polynomials of two variables with integer coefficients
   the coefficient of x^i y^j is in the (i-1,j-1) entry of the array
   this int64 version only allows the coefficients to be 64-bit, not
   the degree, etc.
*)

type poly64 = Poly of int64 array array

(* create a polynomial of degree d *)
let create d =
  let a = Array.create (d+1) (Array.create 1 Int64.zero) in
  for i=0 to d do
    a.(i) <- Array.create (d+1) Int64.zero
  done;
  Poly(a)

let get_a = function
    Poly(a) -> a

(* this is the maximal degree that a polynomial can handle *)
let max_degree p =
  (Array.length (get_a p)) - 1

let get_coeff p i j =
  let d = max_degree p in
  if i > d || j > d then Int64.zero
  else (get_a p).(i).(j)

let set_coeff p i j x =
  (*let d = max_degree p in
  assert (i<=d && j<= d);*)
  (get_a p).(i).(j) <- x

let iter f p =
  for j=0 to max_degree p do
    for i=0 to max_degree p do
      f (get_coeff p i j) i j
    done;
  done

let createij f n =
  let p = create n in
  iter (
    fun c i j ->
      set_coeff p i j (f i j)
  ) p;
  p

let degree p =
  let d = ref 0 in
  iter (
    fun c i j ->
      if c<>Int64.zero && i+j > !d then d := i+j
  ) p;
  !d

let combine_element_wise f p q =
  let r = max (max_degree p) (max_degree q) in
  createij (
    fun i j ->
      f (get_coeff p i j) (get_coeff q i j)
  ) r

let sum = combine_element_wise (Int64.add)
let diff = combine_element_wise (Int64.sub)

let mul p q =
  let r = create ((max_degree p) + (max_degree q)) in
  (* add the product of q times the monomial c x^i y^j to r *)
  let add_monomial_prod c i j =
    iter (
      fun d x y ->
	set_coeff r (i+x) (j+y)
	  (Int64.add (Int64.mul c d) (get_coeff r (i+x) (j+y)))
    ) q
  in
  iter add_monomial_prod p;
  r

let scalar_mul s p =
  createij (
    fun i j ->
      Int64.mul s (get_coeff p i j)
  ) (max_degree p)

let is_zero p =
  let b = ref true in
  iter (
    fun d x y -> if d <> Int64.zero then b := false
  ) p;
  !b

let is_same p1 p2 =
  is_zero (diff p1 p2)

let zero =
  let p = create 0 in
  set_coeff p 0 0 Int64.zero;
  p

let one =
  let p = create 0 in
  set_coeff p 0 0 Int64.one;
  p

let x =
  let p = create 1 in
  set_coeff p 1 0 Int64.one;
  p

let y =
  let p = create 1 in
  set_coeff p 0 1 Int64.one;
  p

let rec pow p n =
  if n = 0 then
    one
  else
    mul p (pow p (n-1))

let print p =
  let d = max_degree p in
  for j=0 to d do
    for i=0 to d do
      let c = get_coeff p i j in
      if c=Int64.zero then (
	if i=0 && j=0 && degree p = 0 then
	  Printf.printf "0 "
      )
      else (
	if c <> Int64.one || (i=0 && j=0) then
	  Printf.printf "%Ld " c;
	if i > 0 then
	  if i > 1 then
	    Printf.printf "x^%d " i
	  else
	    Printf.printf "x ";
	if j > 0 then
	  if j > 1 then
	    Printf.printf "y^%d " j
	  else
	    Printf.printf "y ";
	Printf.printf "+ "
      )
    done;
  done;
  Printf.printf "\n";
  ()

let fprint_mat ch p =
  let d = degree p in
  for i = 0 to d do
    for j = 0 to d do
      Printf.fprintf ch "%Ld\t" (get_coeff p i j)
    done;
  Printf.fprintf ch "\n";
  done

let mat_to_file p fname =
  let co = open_out fname in
  fprint_mat co p;
  close_out co


let xlessdy d =
  let p = create 1 in
  set_coeff p 1 0 Int64.one;
  set_coeff p 0 1 (Int64.neg d);
  p
