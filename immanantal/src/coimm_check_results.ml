let () =

  let max_leaves = 17 in
  let trees = Array.create (max_leaves+1)
    (Array.create 1 (Stree64.create ())) in

  (* note that the trees array is actually indexed by the number of
     leaves, not one less than it! Therefore split goes from 1 to
     n_leaves/2. *)

  (* first generate the array of trees*)

  let s_ch = open_out "status.txt" in
  let progress = ref 0 in

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
    Printf.fprintf s_ch "Constructed %d trees of %d leaves...\n" n_trees n_leaves;
    flush s_ch;

    let cp = Array.map Stree64.to_adj_cp newt in

    for i=0 to n_trees-1 do
      for j=(i+1) to n_trees-1 do
        if cp.(i) = cp.(j) then (
          if !progress < 1 then ( (* this is our first such *)
            Check.evidence "co-adj_imm" newt.(i) newt.(j);
            incr progress;
            ) ;
            if Stree64.co_adj_imm true newt.(i) newt.(j) then (
              if !progress < 2 then ( (* this is our first such *)
                Check.evidence "ex-adj_imm" newt.(i) newt.(j);
                incr progress;
                ) ;
                if Stree64.coimm false newt.(i) newt.(j) then (
                  if !progress < 3 then ( (* this is our first such *)
                    Check.evidence "co-imm" newt.(i) newt.(j);
                    incr progress;
                  ) ;
                  if Stree64.coimm true newt.(i) newt.(j) then (
                    if !progress < 4 then ( (* this is our first such *)
                      Check.evidence "ex-imm" newt.(i) newt.(j);
                      exit 0;
                      ) ;
                    )
                  )
                )
            )
      done;
    done;
    done;
    close_out s_ch;
    ()
