let tree_to_file t fname =
  let ch = open_out fname in
  Printf.fprintf ch "%s;\n" (Stree64.to_newick t);
  close_out ch;
  ()

let evidence results_dir b1 b2 =
  (* n_matchings and ip's *)
  tree_to_file b1 (results_dir^"/a.tre");
  tree_to_file b2 (results_dir^"/b.tre");
  let k = ref 0 in
  let nm = open_out (results_dir^"/n_matchings.txt") in
  Printf.fprintf nm "k\ttree1\ttree2\tforest1\tforest2\n";
  try
    while true do
      let tree_ip1 = Stree64.sub_immanental !k false b1 in
      let tree_ip2 = Stree64.sub_immanental !k false b2 in
      Poly64.mat_to_file tree_ip1
      (Printf.sprintf "%s/a/tree_ip%d.txt" results_dir !k);
      Poly64.mat_to_file tree_ip2
      (Printf.sprintf "%s/b/tree_ip%d.txt" results_dir !k);
      let forest_ip1 = Stree64.sub_immanental !k true b1 in
      let forest_ip2 = Stree64.sub_immanental !k true b2 in
      Poly64.mat_to_file forest_ip1
      (Printf.sprintf "%s/a/forest_ip%d.txt" results_dir !k);
      Poly64.mat_to_file forest_ip2
      (Printf.sprintf "%s/b/forest_ip%d.txt" results_dir !k);
      Printf.fprintf nm "%d\t%Ld\t%Ld\t%Ld\t%Ld\n"
      !k
      (Stree64.n_matchings !k false b1)
      (Stree64.n_matchings !k false b2)
      (Stree64.n_matchings !k true b1)
      (Stree64.n_matchings !k true b2);
      flush nm;
      if Poly64.is_zero tree_ip1 && Poly64.is_zero tree_ip2 then (
        close_out nm;
        raise Exit;
      );
      flush_all ();
      incr k;
    done;
  with Exit -> ();
  ()

(*
let () =
  evidence "bm_results" Stree64.bm1 Stree64.bm2
  *)
