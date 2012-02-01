
module type MBASE =
sig
  type mat
  type t
  val create: int -> int -> mat
  val create_and_set: int -> int -> t -> mat
  val get: mat -> int -> int -> t
  val set: mat -> int -> int -> t -> unit
  val of_arr_arr: t array array -> mat
  val n_rows: mat -> int
  val n_cols: mat -> int
  val equal: mat -> mat -> bool
end

module Matrix (N: Number.NUMBER) (B: MBASE with type t = N.t) =
struct
  type mat = B.mat
  type t = N.t
  let create = B.create
  let create_and_set = B.create_and_set
  let get = B.get
  let set = B.set
  let of_arr_arr = B.of_arr_arr
  let n_rows = B.n_rows
  let n_cols = B.n_cols
  let equal = B.equal

    (* -- basic ocaml ops -- *)
  let iter f m =
    for i=0 to (n_rows m)-1 do
      for j=0 to (n_cols m)-1 do
	f (get m i j)
      done;
    done
  let iterij f m =
    for i=0 to (n_rows m)-1 do
      for j=0 to (n_cols m)-1 do
	f i j (get m i j)
      done;
    done
  let map f m =
    let mp = create (n_rows m) (n_cols m) in
    iterij (fun i j x -> set mp i j (f x)) m;
    mp
  let init n_r n_c f =
    let m = create n_r n_c in
    iterij (fun i j x -> set m i j (f i j)) m;
    m

    (* -- basic matrices -- *)
  let zero n_r n_c = create_and_set n_r n_c N.zero
  let id n =
    let m = zero n n in
    for i=0 to (n_rows m)-1 do
      set m i i N.one
    done;
    m
  let diag_f f n =
    let m = zero n n in
    for i=0 to (n_rows m)-1 do
      set m i i (f i)
    done;
    m
  let number_mat n_r n_c =
    let m = zero n_r n_c in
    iterij (
      fun i j x ->
	set m i j
	  (N.succ (
	     N.add
	       (N.mul
		  (N.of_int i)
		  (N.of_int n_c)
	       )
	       (N.of_int j)
	   ))
    ) m;
    m
  let permutation_matrix a =
    let n = Array.length a in
    let m = zero n n in
    for i=0 to n-1 do
      set m a.(i) i N.one
    done;
    m
      (* of_array fills the matrix up from left to right then top to bottom *)
  let of_array a n_r n_c =
    assert (Array.length a = n_r * n_c);
    init n_r n_c (
      fun i j -> a.(j+i*n_c)
    )
  let of_col_vect a =
    of_array a (Array.length a) 1
  let of_row_vect a =
    of_array a 1 (Array.length a)
  let int_rand_mat max nr nc =
    init nr nc (fun i j -> N.of_int (Random.int max))
  let float_rand_mat max nr nc =
    init nr nc (fun i j -> N.of_float (Random.float max))

  (* -- scalars -- *)
  let scalar_add m s =
    map (fun x -> N.add x s) m
  let scalar_sub m s =
    map (fun x -> N.sub x s) m
  let scalar_mul m s =
    map (fun x -> N.mul x s) m
  let scalar_div m s =
    map (fun x -> N.div x s) m

  (* -- el_wise -- *)
  let apply_el_wise f p q =
    if n_rows p = n_rows q && n_cols p = n_cols q then
      let r = create (n_rows p) (n_cols p) in
      for i=0 to (n_rows p)-1 do
	for j=0 to (n_cols p)-1 do
	  B.set r i j (f (B.get p i j) (B.get q i j) )
	done;
      done;
      r
    else failwith "apply_el_wise : dimensions incompatible"
  let add p q = apply_el_wise N.add p q
  let sub p q = apply_el_wise N.sub p q
  let mul_el_wise p q = apply_el_wise N.mul p q
  let div_el_wise p q = apply_el_wise N.div p q

    (* -- cross_wise -- *)
  let apply_cross_wise f p q =
    if n_cols p = n_rows q then
      let r = create_and_set (n_rows p) (n_cols q) N.zero in
      for i=0 to (n_rows p)-1 do
	for j = 0 to (n_cols q)-1 do
	  for k = 0 to (n_cols p)-1 do
	    set r i j (
	      N.add
		(get r i j)
		( f (get p i k) (get q k j) )
	    )
	    done;
	  done;
	done;
	r
    else failwith "apply_el_wise : dimensions incompatible"
  let mul p q = apply_cross_wise N.mul p q

    (* -- basic matrix ops -- *)
  let assert_square_mat m =
    assert((n_rows m) = (n_cols m))
  let trace m =
    assert_square_mat m;
    let tr = ref N.zero in
    for i=0 to (n_rows m)-1 do
      tr := N.add !tr (get m i i)
    done;
    !tr
  let transpose m =
    init (n_cols m) (n_rows m) (fun i j -> get m j i)
  let is_zero m =
    try
      iter (
	fun x -> if x <> N.zero then raise Exit
      ) m;
      true
    with
	Exit -> false
  let add_row m row =
    assert(Array.length row = n_cols m);
    init (1+(n_rows m)) (n_cols m) (
      fun i j ->
	if i=0 then
	  row.(j)
	else
	  get m (i-1) j
    )
  let add_col m col =
    assert(Array.length col = n_rows m);
    init (n_rows m) (1+(n_cols m)) (
      fun i j ->
	if j=0 then
	  col.(i)
	else
	  get m i (j-1)
    )
  let mul_vec m v =
    assert(Array.length v = n_cols m);
    let result = Array.create (n_rows m) N.zero in
    for i=0 to (n_rows m)-1 do
      for j=0 to (n_cols m)-1 do
	result.(i) <- N.add result.(i) (N.mul (get m i j) v.(j))
      done;
    done;
    result
      (* bilinear form *)
  let bf m v w =
    assert(Array.length v = n_rows m);
    assert(Array.length w = n_cols m);
    let tot = ref N.zero in
    iterij (
      fun i j x ->
	tot := N.add !tot (N.mul v.(i) (N.mul w.(j) x))
    ) m;
    !tot
      (* quadratic form *)
  let qf m v =
    let n = Array.length v in
    assert(n = n_rows m);
    assert(n = n_cols m);
    let tot = ref N.zero in
    for i=0 to n-1 do
      tot := N.add !tot (N.mul v.(i) (N.mul v.(i) (get m i i) ) )
    done;
    for i=0 to n-1 do
      for j=i+1 to n-1 do
	tot :=
	  N.add
	    !tot
	    (N.mul v.(i)
	       (N.mul v.(j) (N.add (get m i j) (get m j i) ) ) )
      done;
    done;
    !tot
      (* quadratic form for symmetric matrices *)
  let qf_symm m v =
    let n = Array.length v in
    assert(n = n_rows m);
    assert(n = n_cols m);
    let tot = ref N.zero in
    for i=0 to n-1 do
      tot := N.add !tot (N.mul v.(i) (N.mul v.(i) (get m i i) ) )
    done;
    for i=0 to n-1 do
      for j=i+1 to n-1 do
	let e = get m i j in
	tot := N.add !tot (N.mul v.(i) (N.mul v.(j) (N.add e e ) ) )
      done;
    done;
    !tot
(* delete rows and columns *)
  let del_rc m rows cols =
    let nr = (n_rows m) - (List.length rows) in
    let nc = (n_cols m) - (List.length cols) in
    assert(nr > 0 && nc > 0);
    let n = create nr nc in
    let rcount = ref 0 in
    for i=0 to (n_rows m)-1 do
      if not (List.mem i rows) then (
	let ccount = ref 0 in
	for j=0 to (n_cols m)-1 do
	  if not (List.mem j cols) then (
	    set n !rcount !ccount (get m i j);
	    incr ccount;
	  )
	done;
	incr rcount;
      )
    done;
    n


(* del_rows *)
  let del_rows m rows =
    del_rc m rows []
(* del_cols *)
  let del_cols m cols =
    del_rc m [] cols
(* the matrix with row i and column j crossed out *)
  let pre_minor_mat m i j =
    del_rc m [i] [j]

(* permute rows and columns. ar and ac are arrays giving the permutation *)
  let permute_rc m ar ac =
    init (n_rows m) (n_cols m) (fun i j -> get m ar.(i) ac.(j))
  (* permute rows and colums. the permutations are encoded as
     transposition pairs *)
  let permute_rc_transpos m pr pc =
    let nr = n_rows m in
    let nc = n_cols m in
    let make_perm_arr pl n =
      let a = Array.create n 0 in
      for i=0 to n-1 do a.(i) <- i done;
      List.iter (fun (r,s) ->
		   let t = a.(r) in
		   a.(r) <- a.(s);
		   a.(s) <- t
		) pl;
      a
    in
    let ar = make_perm_arr pr nr in
    let ac = make_perm_arr pc nc in
    init nr nc (fun i j -> get m ar.(i) ac.(j))

  (* -- IO -- *)
  let write ch m =
    let widths = Array.create (n_cols m) 0 in
    for j=0 to (n_cols m)-1 do
      for i=0 to (n_rows m)-1 do
	let width = String.length (N.to_string (get m i j)) in
	if width > widths.(j) then
	  widths.(j) <- width
      done;
    done;
    for i=0 to (n_rows m)-1 do
      for j=0 to (n_cols m)-1 do
	let s = N.to_string (get m i j) in
	Printf.fprintf ch "%s" s;
	for i=0 to widths.(j) - (String.length s) do
	  Printf.fprintf ch " ";
	done;
      done;
      Printf.fprintf ch "\n";
    done;
    ()
  let write_tabbed ch m =
    for i=0 to (n_rows m)-1 do
      for j=0 to (n_cols m)-1 do
	Printf.fprintf ch "%s\t" (N.to_string (get m i j));
      done;
      Printf.fprintf ch "\n";
    done;
    ()
  let write_math ch m =
    let a = (n_rows m)-1 in
    let b = (n_cols m)-1 in
    Printf.fprintf ch "{";
    for i=0 to a do
      Printf.fprintf ch "{";
      for j=0 to b do
	Printf.fprintf ch "%s" (N.to_string (get m i j));
	if j<b then
	  Printf.fprintf ch ", "
	else
	  Printf.fprintf ch "}"
      done;
      if i<a then
	Printf.fprintf ch ", "
      else
	Printf.fprintf ch "}"
    done;
    Printf.fprintf ch "\n";
    ()
  let print = write stdout
  let print_tabbed = write_tabbed stdout
  let print_math = write_math stdout
  let to_file_gen m f fname =
    let ch = open_out fname in
    f ch m;
    close_out ch
  let to_aligned_file m fname = to_file_gen m write fname
  let to_file m fname = to_file_gen m write_tabbed fname

  let of_channel cin =
    let split_on_space s =
      let is_space c = (c = '\t' || c = ' ' || c = '\n') in
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
    in
    let rec readlines acc =
      try readlines ((input_line cin)::acc)
      with End_of_file -> List.rev acc
    in
    let arr_of_line str =
      Array.of_list (
	List.map N.of_string (
	  split_on_space str
	)
      )
    in
    B.of_arr_arr (
      Array.of_list (
	List.map arr_of_line (
	  readlines []
	)
      )
    )

  let of_file fname =
    let cin = open_in fname in
    let m = of_channel cin in
    close_in cin;
    m

    (* -- characteristic polynomial -- *)
    (* apply Leverrier-Faddeev algorithm *)
    (* returns an array such that a.(k) is the coeff of x^{n-k} *)
    let char_poly m =
      assert_square_mat m;
      let n = n_rows m in
      let p = Array.create (n+1) N.one in
      let rec calc_b k prev_mb =
	let b_k = add prev_mb (diag_f (fun i -> p.(k-1)) n) in
	(*print b_k;
	  Printf.printf "\n";*)
	if k = n+1 then
	  assert(is_zero(b_k))
	else (
	  let mb_k = mul m b_k in
	  p.(k) <- N.neg (N.div (trace mb_k) (N.of_int k));
	  (* Printf.printf "%s\n\n" (N.to_string p.(k));*)
	  calc_b (k+1) mb_k
	)
      in
      calc_b 1 (zero n n);
      p

  let det m =
    assert_square_mat m;
    let n = n_rows m in
    let cp = char_poly m in
    if n mod 2 = 0 then
      cp.(n)
    else
      N.neg cp.(n)

(* quite inefficient, a much more intelligent way would be to use the
inverse matrix. *)
  let minors m =
    assert_square_mat m;
    let n = n_rows m in
    init n n (
      fun i j -> det (pre_minor_mat m i j)
    )
  let signed_minors m =
    assert_square_mat m;
    let n = n_rows m in
    let sgn n x = if n mod 2 = 0 then x else N.neg x in
    init n n (
      fun i j -> sgn (i+j) (det (pre_minor_mat m i j))
    )

end

module ArrArr (N: Number.NUMBER) =
  struct
    type t = N.t
    type mat = N.t array array
    let create n m = Array.create_matrix n m N.zero
    let create_and_set n m e = Array.create_matrix n m e
    let get m i j = m.(i).(j)
    let set m i j e = m.(i).(j) <- e
    let of_arr_arr m = m
    let n_rows m = Array.length m
    let n_cols m = Array.length m.(0)
    let equal = (=)
end

module CBigMatR =
  struct
    type t = float
    type mat =
	(float, Bigarray.float64_elt, Bigarray.c_layout)
	Bigarray.Array2.t
    let create n m =
      Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout n m
    let get = Bigarray.Array2.get
    let set = Bigarray.Array2.set
    let create_and_set n m e =
      let a = create n m in
      for i=0 to n-1 do
	for j=0 to m-1 do
	 set a i j e
	done;
      done;
      a
    let of_arr_arr =
      Bigarray.Array2.of_array Bigarray.float64 Bigarray.c_layout
    let n_rows = Bigarray.Array2.dim1
    let n_cols = Bigarray.Array2.dim2
    let equal = (=)
end

module FBigMatR =
  struct
    type t = float
    type mat =
	(float, Bigarray.float64_elt, Bigarray.fortran_layout)
	Bigarray.Array2.t
    let create n m =
      Bigarray.Array2.create Bigarray.float64
	Bigarray.fortran_layout n m
    let get m i j = Bigarray.Array2.get m (i+1) (j+1)
    let set m i j e = Bigarray.Array2.set m (i+1) (j+1) e
    let create_and_set n m e =
      let a = create n m in
      for i=0 to n-1 do
	for j=0 to m-1 do
	 set a i j e
	done;
      done;
      a
    let of_arr_arr =
      Bigarray.Array2.of_array Bigarray.float64
	Bigarray.fortran_layout
    let n_rows = Bigarray.Array2.dim1
    let n_cols = Bigarray.Array2.dim2
    let equal = (=)
  end

module CBMR = Matrix (Number.R) (CBigMatR)
module FBMR = Matrix (Number.R) (FBigMatR)

module AAMatrix (N: Number.NUMBER) =
  Matrix (N) (ArrArr (N))

module AAB = AAMatrix (Number.B)
module AAZ = AAMatrix (Number.Z)
module AAZ64 = AAMatrix (Number.Z64)
module AAR = AAMatrix (Number.R)

