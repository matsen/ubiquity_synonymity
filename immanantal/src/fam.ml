
let int_of_bool b =
  if b then 1 else 0

let int_div i j =
  (float_of_int i) /. (float_of_int j)

let round_up x =
  int_of_float (ceil x)

let round_down x =
  int_of_float (floor x)

let list_max l =
  List.fold_left max (List.hd l) l

let sum_float_list l =
  List.fold_left (+.) 0. l

let avg_float_list l =
  (sum_float_list l) /. (float_of_int (List.length l))

let print_list fmt l =
  List.iter (fun x -> Printf.printf fmt x) l;
  Printf.printf "\n"

let print_int_list = print_list "%d\t"
let print_float_list = print_list "%f\t"

let print_arr fmt a =
  Array.iter (fun x -> Printf.printf fmt x) a;
  Printf.printf "\n"

let print_int_arr = print_arr "%d\t"
let print_float_arr = print_arr "%f\t"

let atob_list a b =
  assert (a <= b);
  let rec atbl a b l =
    if a > b then l else atbl a (b-1) (b::l)
  in  (* note above... tail rec means do things backwards...*)
  atbl a b [];;

let rec list_create n x =
  if n < 1 then [] else x::(list_create (n-1) x)

let list_remove l_orig rem_i =
  assert (rem_i >= 0 && rem_i < (List.length l_orig));
  let the_x = ref (List.hd l_orig) in
  let rec aux l i =
  match l with
      x::lp ->
	if i <> rem_i then
	  x::(aux lp (i+1))
	else (
	  the_x := x;
	  aux lp (i+1) (* skip x *)
	)
    | [] -> []
  in
  (!the_x, aux l_orig 0)

let list_of_array a =
  let add l x = x::l in
  List.rev (Array.fold_left add [] a)

let split_on_space s =
  let is_space c = (c = '\t' || c = ' ') in
  let len = String.length s in
  let on_string = ref false in
  let start_pos = ref 0 in
  let substrs = ref [] in
  let attempt_terminate i =
    if !on_string then (
      substrs := (String.sub s !start_pos (i - !start_pos))
      ::!substrs;
      on_string := false;
    )
  in
  for i=0 to len-1 do
    if is_space s.[i] then (
      attempt_terminate i
    )
    else (
      if not !on_string then (
        on_string := true;
        start_pos := i
      )
    )
  done;
  attempt_terminate len;
  List.rev !substrs

