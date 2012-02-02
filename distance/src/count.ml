
let create = Hashtbl.create

let how_many h a = 
  try 
    Hashtbl.find h a 
  with Not_found -> 0

let add h x = 
  let n = how_many h x in
  if n <> 0 then
    Hashtbl.remove h x;
  Hashtbl.add h x (n+1)

let of_list l = 
  (* below: guess that 10% of elts are same *)
  let h = create (9*(List.length l)/10) in
  List.iter (fun x -> add h x) l;
  h

let as_list h = Hashtbl.fold (fun k v l -> (k,v)::l) h []

let n_distinct h = Hashtbl.fold (fun k v n -> (n+1)) h 0

let to_file printer fname h = 
  let ch = open_out fname in
  Hashtbl.iter (
    fun k v ->
      Printf.fprintf ch "%d\t" v;
      printer ch k;
      Printf.fprintf ch "\n";
  ) h;
  close_out ch

let profile h = 
  List.sort (fun x y -> -(compare x y)) (
    Hashtbl.fold (fun k v l -> v::l) h []
  )

let sample = of_list [1;2;3;4;5;6;7;1;2;3;4;5;6;1;2;3;4;5;1;2;3;4;1;2;3;1;2;1]    
      
