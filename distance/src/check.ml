
(* "cloaking" refers to building a matrix out of a row, a column, and a matrix
 * as in Lemma 4 (the exchange property for distance matrices) *)
let cloak m row col =
  let n_rows = Mat.AAZ64.n_rows m in
  let n_cols = Mat.AAZ64.n_cols m in
  assert(Array.length col = n_cols);
  assert(Array.length row = n_rows);
  Mat.AAZ64.init (n_rows+1) (n_cols+1) (
    fun i j ->
      if i=n_cols && j=n_rows then Int64.zero
      else if i=n_cols then row.(j)
      else if j=n_rows then col.(i)
      else Mat.AAZ64.get m i j
      )

let print_poly ch p =
  Array.iter (fun x -> Printf.fprintf ch "%Ld\t" x) p;
  Printf.fprintf ch "\n"

let check b1 b2 =
  let m1 = Stree64dist.to_dist_int64 b1 in
  let m2 = Stree64dist.to_dist_int64 b2 in
  let a1 = Stree64dist.dists_to_root b1 in
  let a2 = Stree64dist.dists_to_root b2 in
  let one = Array.make (Stree64dist.n_leaves b1) Int64.one in

  let ch1 = open_out "check_results/dist_poly1.txt" in
  let ch2 = open_out "check_results/dist_poly2.txt" in

  let test_mats m1 m2 =
    let cp1 = Mat.AAZ64.char_poly m1 in
    let cp2 = Mat.AAZ64.char_poly m2 in

    if cp1 <> cp2 then (failwith "Characteristic polys not equal!");

    print_poly ch1 cp1;
    print_poly ch2 cp1;
    ()
    in

    test_mats m1 m2;
    test_mats (cloak m1 a1 a1) (cloak m2 a2 a2);
    test_mats (cloak m1 a1 one) (cloak m2 a2 one);
    test_mats (cloak m1 one a1) (cloak m2 one a2);
    test_mats (cloak m1 one one) (cloak m2 one one);

    close_out ch1;
    close_out ch2;
    ()

  let () =

    check Stree64dist.bm1 Stree64dist.bm2 ;

    let print_tree fname t =
      let ch = open_out fname in
      Printf.fprintf ch "%s;\n" (Stree64dist.to_newick t);
      close_out ch;
      ()
      in

      print_tree "check_results/bm1.nex" Stree64dist.bm1;
      print_tree "check_results/bm2.nex" Stree64dist.bm2;

      ()
