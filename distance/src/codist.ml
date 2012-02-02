
let () =

  let max_leaves = 18 in
  let trees = Array.create (max_leaves+1)
    (Array.create 1 (Stree64dist.create ())) in

  let sch = open_out "n_spectra_results/n_spectra.out" in
  Printf.fprintf sch "leaves\ttrees\tspectra\n";
  let pch = open_out "n_spectra_results/spectra_profile.out" in


  (* note that the trees array is actually indexed by the number of
     leaves, not one less than it! Therefore split goes from 1 to
     n_leaves/2. *)

  (* first generate the array of trees*)

  for n_leaves=2 to max_leaves do
    let newtrees = ref [] in
    for split=1 to n_leaves/2 do
      let left = trees.(n_leaves-split) in
      let right = trees.(split) in
      if ((n_leaves mod 2) = 0) && ((n_leaves/2) = split) then (
	(* we have equal depth on either side, so let's not duplicate *)
	for i=0 to Array.length left - 1 do
	  for j=i to Array.length right - 1 do
	    (* j=i for non-dup *)
	    let nt = Stree64dist.join2 left.(i) right.(j) in
	    newtrees := nt::!newtrees;
	  done;
	done
      )
      else (
	for i=0 to (Array.length left) - 1 do
	  for j=0 to (Array.length right) - 1 do
	    let nt = Stree64dist.join2 left.(i) right.(j) in
	    newtrees := nt::!newtrees;
	  done;
	done;
      );
    done;
    let newt = Array.of_list !newtrees in
    trees.(n_leaves) <- newt;

    let n_trees = Array.length trees.(n_leaves) in
    Format.printf "Constructed %d trees of %d leaves...\n" n_trees n_leaves;
    flush stdout;

    let c = Count.of_list
      (List.map Mat.AAZ64.char_poly
	 (List.map Stree64dist.to_dist_int64 !newtrees)) in
    Printf.fprintf sch "%d\t%d\t%d\n"
      n_leaves
      n_trees
      (Count.n_distinct c);
    flush sch;
    let prof = Count.of_list (Count.profile c) in
    List.iter (fun (x,y) -> Printf.fprintf pch "(%d,%d)\t" y x) (Count.as_list prof);
    Printf.fprintf pch "\n";
    flush pch;
  done;

  close_out sch;
  close_out pch;
  ()



(*

    let poly = Array.map (
      fun t ->
	let d = to_dist_int64 t in
	let p = Mat.AAZ64.char_poly (to_dist_int64 t) in
	(*
	  Mat.AAZ64.print d;
	  Stree64.print_newick t;
	  Printf.printf "\n";
	  Array.iter (fun x -> Printf.printf "%Ld\t" x) p;
	  Printf.printf "\n";
	*)
	p
    ) newt in

    for i=0 to n_trees-1 do
      for j=(i+1) to n_trees-1 do
	if poly.(i) = poly.(j) then (
	  Printf.printf "Co-dist found!\n";
	  Array.iter (fun x -> Printf.printf "%Ld\t" x) poly.(i);
	  Printf.printf "\n";
	  let ch_nex = open_out "results/codista.nex" in
	  Printf.fprintf ch_nex "%s;\n" (Stree64.to_newick newt.(i));
	  close_out ch_nex;
	  let ch_nex = open_out "results/codistb.nex" in
	  Printf.fprintf ch_nex "%s;\n" (Stree64.to_newick newt.(j));
	  close_out ch_nex;
	  (* nodelists *)
	  let ch_nla = open_out "results/codist_nodelista.txt" in
	  Stree64.write_nodelist ch_nla newt.(i);
	  close_out ch_nla;
	  let ch_nlb = open_out "results/codist_nodelistb.txt" in
	  Stree64.write_nodelist ch_nlb newt.(j);
	  close_out ch_nlb;
	  Mat.AAZ64.to_file (to_dist_int64 newt.(i)) "results/distmata.txt";
	  Mat.AAZ64.to_file (to_dist_int64 newt.(j)) "results/distmatb.txt";
	  exit (0);
	)
      done;
    done;
*)
