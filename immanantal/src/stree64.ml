(* uses 64 bit integers for matchings and immanental poly's *)

(*
 * NOTE:
 *
 * the Mat FBMR layout has changed from column-major to row-major layout
 * see Bigmat and Mat.FBMR for details
 *
 *)
type stree = Node of (stree list)

let get_l = function Node(l) -> l

let create () = Node([])

let join l = Node (l)

let join2 t s = Node ([t;s])

let rec copy = function
    Node(l) -> Node(List.map copy l)

let rec comb n =
  if n = 1 then
    create ()
  else
    join2 (comb (n-1)) (create ())

let rec balanced n =
  if n = 1 then
    create ()
  else
    let half = Fam.int_div n 2 in
    join2
      (balanced (Fam.round_up half))
      (balanced (Fam.round_down half))

(* the linear graph on n edges (not n nodes) *)
let rec line n =
  if n < 1 then
    create ()
  else
    join [line (n-1)]

(* substitute t1 at every leaf of t2 *)
let extend_leaves t1 t2 =
  let rec aux t =
    if get_l t = [] then
      copy t1
    else
      match t with
	  Node(l) -> Node(List.map aux l)
  in
  aux t2

let change () =
  extend_leaves (balanced 4) (comb 3)

let rec n_edges = function
    Node(l) ->
      if l = [] then 0
      else List.fold_left (+)
        (List.length l) (List.map n_edges l)

let rec n_leaves = function
    Node(l) ->
      if l = [] then 1
      else List.fold_left (+) 0 (List.map n_leaves l)

let rec to_newick = function
    Node(l) ->
      if l <> [] then
	let subtrees = List.map to_newick l in
	Printf.sprintf "(%s)" (String.concat "," subtrees)
      else
	Printf.sprintf "x"

let print_newick t =
  Printf.printf "%s\n" (to_newick t)

let to_pairlist t =
  let master_num = ref 1 in
  let rec pairize prev_num t =
    match t with
	Node(l) -> (
	  incr master_num;
	  let my_num = !master_num in
	  let novel = List.map (pairize my_num) l in
	  (prev_num, my_num)::(List.flatten novel)
	)
  in
  List.flatten (List.map (pairize 1) (get_l t))

(* write the pairlist in a format sutable for ME! *)
let write_pairlist ch t =
  List.iter (
    fun (a,b) -> Printf.fprintf ch "%d\t%d\n" a b
  ) (to_pairlist t )

(* write the pairlist in a format suitable for mathematica *)
let write_math_pairlist ch t =
  let print_pair = function
      (a,b) -> Printf.fprintf ch "{%d, %d}" a b
  in
  let pp_comma p = Printf.fprintf ch ", "; print_pair p in
  Printf.fprintf ch "{";
  match (to_pairlist t) with
      p::l ->
	print_pair p;
	List.iter pp_comma l;
	Printf.fprintf ch "}"
    | [] -> ()

let to_nodelist t =
  let master_num = ref (-1) in
  let rec nodify t =
    match t with
        Node(l) -> (
          incr master_num;
          let my_num = !master_num in
          let novel = List.map nodify l in
          let child_nums = List.map fst novel in
          let child_lists = List.map snd novel in
          (my_num, (my_num, child_nums)::(List.flatten child_lists))
        )
  in
  snd (nodify t)

let write_nodelist ch t =
  let nl = to_nodelist t in
  let print_node = function
      (num, l) ->
        Printf.fprintf ch "%d \t * " num;
        List.iter (Printf.fprintf ch "%d ") l;
        Printf.fprintf ch "\n"
  in
  List.iter print_node nl

let to_adj_mat t =
  let n = n_leaves t in
  let m = Mat.FBMR.create_and_set (2*n-1) (2*n-1) 0. in
  let count = ref 0 in
  let rec aux = function
    | Node(l) ->
        let my_i = !count in
        incr count;
        List.iter
                (fun i ->
                  (*Printf.printf "(%d,%d)\n" my_i i;*)
                  Mat.FBMR.set m my_i i 1.;
                  Mat.FBMR.set m i my_i 1.;
                  () )
                (List.map aux l);
        my_i
  in
  let _ = aux t in
  m

let to_adj_cp t = Mat.FBMR.char_poly (to_adj_mat t)


let half_ceil n =
  if n mod 2 = 0 then n/2
  else (n+1)/2

(* red specifies if a given vertex is already part of a matching *)
let n_matchings k red t =
  let zero = Int64.zero in
  let one = Int64.one in
  let rec total f k =
    if k=0 then f 0 else Int64.add (f k) (total f (k-1)) in
  let rec nm k red t =
    if k = 0 then one
    else if k < 0 then zero
    else
      (* soap stands for sum over all partitions. k is the number of
	 matchings left to make. red is whether the current node is
	 colored or not. we distribute k in all possible ways over the
	 subtrees and compute the total number of matchings for this
	 subtree. *)
      let rec soap k red subtrees =
	if k < 0 then zero else
	  match subtrees with
	      x::l ->
		(* we can match at most ceil(e/2) edges, where e is the
                   total number of edges *)
		let max_k_x = half_ceil (n_edges x) in
		(* similarly, *)
		let max_k_l =
                  half_ceil (
                    List.fold_left (+) 0 (List.map n_edges l))
		in
		total (
		  fun i ->
		    (* the upper edge is red, and we need to pass that
		       on to soap. however, num is called for the next
		       subtree down, and that is not red (yet) *)
		    (* below: if the number of matchings is greater than half of the
		       number of edges (note that the one plus is because we can match the
		       "invisible" edge going up from x) then we won't get any matchings and
		       it's faster to say zero *)
                    if (k-i > 1 + max_k_l)
                      || (i > 1 + max_k_x)
                      || (l=[] && (k-i)>0)
                    then zero
		    else (
		      Int64.add
			(Int64.mul
			   (soap (k-i) red l)
			   (nm i false x))
			(if red then
			   zero (* we can't color the edge to x *)
			 else
			   (* color the edge going down to x. therefore the
			      edge coming down from the top is now colored and
			      we need to let the rest of soap know. *)
			   (Int64.mul
			      (soap (k-i) true l)
			      (nm (i-1) true x))
			)
		    )
		) k
	    | [] ->
		if k=0 then one else zero
      in
      soap k red (get_l t)
  in
  nm k red t


let sub_immanental k red t =
  let pzero = Poly64.zero in
  let pone = Poly64.one in
  let rec ptotal f k =
    if k=0 then f 0
    else
      let new_one = f k in
      if Poly64.is_zero new_one then
	ptotal f (k-1)
      else
	Poly64.sum (f k) (ptotal f (k-1))
  in
  let si k red t =
    (* coap stands for combine over all partitions. *)
    let rec coap k red subtrees deg =
      if k<0 then pzero
      else
	let rec imm k red t =
	  if k<0 then pzero
	  else
	    let l = get_l t in
	    coap k red l (1+List.length l)
	in
	match subtrees with
	    x::l ->
	      (* we can match at most ceil(e/2) edges, where e is the
                 total number of edges *)
	      let max_k_x = half_ceil (n_edges x) in
	      (* similarly, *)
	      let max_k_l =
                half_ceil (
                  List.fold_left (+) 0 (List.map n_edges l))
	      in
	      ptotal (
		fun i ->

		  if (k-i > 1 + max_k_l)
                    || (i > 1 + max_k_x)
                    || (l=[] && (k-i)>0)
                  then pzero
		  else (
		    if red then
		      Poly64.mul (coap (k-i) true l deg) (imm i false x)
		    else
		      Poly64.sum
			(Poly64.mul (coap (k-i) false l deg) (imm i false x))
			(Poly64.mul (coap (k-i) true l deg) (imm (i-1) true x))
		  )
	      ) k
	  | [] ->
	      if k=0 then
		if red = false then Poly64.xlessdy (Int64.of_int deg) else pone
	      else pzero
    in
    let l = get_l t in
    coap k red l (List.length l) (* no 1+ because we are at root *)
  in
  si k red t


let compute_matchings red t =
  let cont = ref true in
  let matchings = ref [] in
  let k = ref 0 in
  while !cont do
    let n = n_matchings !k red t in
    matchings := n::!matchings;
    cont := ((Int64.compare n Int64.zero) <> 0);
    incr k;
  done;
  Array.of_list (List.rev !matchings)

let adj_imm red t =
  let l = ref [] in
  try
    while true do
      let n = n_matchings (List.length !l) red t in
      l := n::(!l);
      if ((Int64.compare n Int64.zero) = 0) then
        raise Exit;
    done;
    List.rev !l
        with
        Exit -> List.rev !l

let both_adj_imm t =
  (adj_imm false t, adj_imm true t)

let private_co_adj_imm verbose red t1 t2 =
  let cont = ref true in
  let return = ref true in
  let k = ref 0 in
  while !cont do
    let n1 = n_matchings !k red t1 in
    let n2 = n_matchings !k red t2 in
    if n1 <> n2 then (
      if verbose then (
	Printf.printf
	  "The trees differ in the number of %d-matchings:\n" !k;
	Printf.printf
	  "t1 has %Ld matchings and t2 has %Ld matchings.\n" n1 n2;
      );
      cont := false;
      return := false;
    )
    else (
      if verbose then (
	Printf.printf "Both trees have %Ld %d-matchings.\n" n1 !k;
      );
      cont := ((Int64.compare n1 Int64.zero) <> 0);
    );
    flush stdout;
    incr k;
  done;
  !return

let verbose_co_adj_imm = private_co_adj_imm true
let co_adj_imm = private_co_adj_imm false
let both_co_adj_imm t1 t2 =
  (co_adj_imm true t1 t2) && (co_adj_imm false t1 t2)

let private_coimm verbose red t1 t2 =
  let cont = ref true in
  let return = ref true in
  let k = ref 0 in
  while !cont do
    let p1 = sub_immanental !k red t1 in
    let p2 = sub_immanental !k red t2 in
    if not (Poly64.is_same p1 p2) then (
      if verbose then (
	Printf.printf "The trees differ in the %dth polynomial.\n" !k;
	Printf.printf "For the first tree it is:\n";
	Poly64.print p1;
	Printf.printf "For the second tree it is:\n";
	Poly64.print p2;
      );
      cont := false;
      return := false;
    )
    else (
      if verbose then (
	Printf.printf "Trees are the same in the %dth polynomial.\n" !k;
	Poly64.print p1;
      );
      cont := not (Poly64.is_zero p1);
    );
    flush stdout;
    incr k;
  done;
  !return

let verbose_coimm  = private_coimm true
let coimm  = private_coimm false
let both_coimm t1 t2 =
  (coimm true t1 t2) && (coimm false t1 t2)


(* fast because of early rejection *)
let fast_co_adj_imm red t1 t2 =
  let cont = ref true in
  let k = ref 0 in
  try
    while !cont do
      let n1 = n_matchings !k red t1 in
      if n1 <>  (n_matchings !k red t2) then raise Exit;
      cont := ((Int64.compare n1 Int64.zero) <> 0);
    incr k;
    done;
    true
  with
  | Exit -> false

let fast_coimm red t1 t2 =
  let cont = ref true in
  let k = ref 0 in
  try
    while !cont do
      let p1 = sub_immanental !k red t1 in
      let p2 = sub_immanental !k red t2 in
      if not (Poly64.is_same p1 p2) then raise Exit;
      cont := not (Poly64.is_zero p1);
      incr k;
    done;
    true
  with
  | Exit -> false



(* sample trees *)

let rec addline n t =
  if n = 0 then t
  else join [addline (n-1) t]

let rec addwhiskers n t =
  if n = 0 then t
  else join [create (); addwhiskers (n-1) t]

let bis1 =
  balanced 8

let bis2 =
  join [
    join [
      balanced 4;
      create ()
    ];
    comb 3
  ]

let bm1 =
  join [
    join [
      comb 3;
      comb 3
    ];
    addwhiskers 1 (
      join [
	comb 4;
        addwhiskers 2 (
          balanced 4
        )
      ]
    )
  ]

let bm2 =
  join [
    addwhiskers 1 (
      balanced 4
    );
    join [
      comb 4;
      addwhiskers 2 (
	join [
	  comb 3;
	  comb 3
	]
      )
    ]
  ]

let rec print depth t =
  let print_spaces k = for i=1 to k do Printf.printf "  " done in
  if get_l t = [] then (
    print_spaces depth;
    Printf.printf "1.0\n"
  )
  else
    List.iter (print (depth+1)) (get_l t)
