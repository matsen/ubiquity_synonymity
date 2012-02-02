(* uses 64 bit integers for matchings and immanental poly's *)

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

let rec to_newick_unit_edgelength = function
    Node(l) ->
      if l <> [] then 
	let subtrees = List.map to_newick_unit_edgelength l in
	Printf.sprintf "(%s):1.0" (String.concat "," subtrees)
      else
	Printf.sprintf "x:1.0"

let print_newick t = 
  Printf.printf "%s\n" (to_newick t)

let print_newick_unit_edgelength t = 
  Printf.printf "%s\n" (to_newick_unit_edgelength t)

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

let dists_to_root t = 
  let count = ref 0 in
  let d = Array.create (n_leaves t) Int64.zero in
  let rec aux i = function 
    Node(l) -> 
      if l = [] then (
        d.(!count) <- Int64.of_int i;
        incr count;
      )
      else
        List.iter (aux (i+1)) l;
  in
  aux 1 t;
  d

let to_dist_int64 t =
  let n = n_leaves t in
  let a = Mat.AAZ64.create_and_set n n Int64.zero in
  let iter_all_to_all f l1 l2 = 
    let do_2 a = 
      let f_a b = f a b in
      List.iter f_a l2
    in
    List.iter do_2 l1
  in
  let leafnum = ref (-1) in
  let rec td_aux = function
      Node(l) -> 
        if (l = []) then (
          incr leafnum;
          [(!leafnum, Int64.one)] (*generate a "split" *)
        )
        else (
          let splits = List.map td_aux l in
          (* splits is a list of split lists. *)
          
          let add_to_mat s1 s2 = 
            Mat.AAZ64.set a (fst s1) (fst s2) (Int64.add (snd s1) (snd s2))
          in    
	  (* first we add all of the splits to the matrix. we need to
             do this complex all-to-all business because if we flatten
             first then we will be treating sister groups the same as
             groups which are not. for example, say we are at a node
             which is the balanced tree on four leaves with leaves being
             subtrees A, B, C, and D. We need to add the distance between
             A and C; A and D; so on, but we have already in the previous
             step done A and B so we don't want to do that again. *)
          let ll = List.length splits in
          for i=0 to ll-1 do
            for j=0 to ll-1 do
              if i <> j then
                iter_all_to_all 
                  add_to_mat 
                  (List.nth splits i) 
                  (List.nth splits j)
            done;
          done;
          let add_our_edge = function
              (num, dist) -> (num, Int64.succ dist)
          in
          List.flatten (List.map (List.map add_our_edge) splits)
        )
  in
  let _ = td_aux t in
  a

(* sample trees *)

let rec addline n t = 
  if n = 0 then t
  else join [addline (n-1) t]
    
let rec addwhiskers n t = 
  if n = 0 then t
  else join [create (); addwhiskers (n-1) t]
    
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


