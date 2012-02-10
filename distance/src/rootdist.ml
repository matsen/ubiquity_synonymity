
let () =

  let max_leaves = 19 in
  let trees = Array.create (max_leaves+1)
    (Array.create 1 (Stree64dist.create ())) in

  let sch = open_out "n_rootdist.out" in
  Printf.fprintf sch "leaves\ttrees\trootdists\n";


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

    let rl = List.map Stree64dist.dists_to_root !newtrees in
    List.iter (Array.sort compare) rl;
    let c = Count.of_list rl in
    Printf.fprintf sch "%d\t%d\t%d\n"
      n_leaves
      n_trees
      (Count.n_distinct c);
    flush sch;
  done;

  close_out sch;
  ()



