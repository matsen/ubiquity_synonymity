
let () =

  let max_leaves = 20 in
  let trees = Array.create (max_leaves+1)
    (Array.create 1 (Stree64.create ())) in

  let sch = open_out "n_spectra_results/n_spectra.out" in
  Printf.fprintf sch "leaves\ttrees\tspectra\n";
  let pch = open_out "n_spectra_results/spectra_profile.out" in

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
	    let nt = Stree64.join2 left.(i) right.(j) in
	    newtrees := nt::!newtrees;
	  done;
	done
      )
      else (
	for i=0 to (Array.length left) - 1 do
	  for j=0 to (Array.length right) - 1 do
	    let nt = Stree64.join2 left.(i) right.(j) in
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

    let c = Count.create n_trees in
    for i=0 to n_trees-1 do
      Count.add c (Stree64.compute_matchings false newt.(i));
    done;
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
