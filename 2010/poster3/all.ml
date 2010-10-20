
let map = [
  1,   [ (-1, -1); (-1, -1); (2, 4); (-1, -1) ];
  2,   [ (1, 4); (-1, -1); (3, 2); (5, 7) ];
  3,   [ (2, 2); (-1, -1); (-1, -1); (6, 1) ];
  4,   [ (-1, -1); (-1, -1); (5, 5); (7, 2) ];
  5,   [ (4, 5); (2, 7); (6, 2); (8, 1); ];
  6,   [ (5, 2); (3, 1); (-1, -1);  (-1, -1) ];
  7,   [ (-1, -1); (4, 2); (8, 3); (10, 1) ];
  8,   [ (7, 3); (5, 1); (9, 1); (11, 4)];
  9,   [ (8, 1); (-1, -1);  (-1, -1); (12, 4) ];
  10,   [ (-1, -1);  (7, 1); (11, 3); (-1, -1) ];
  11,   [ (10, 3); (8, 4); (12, 5); (14, 1)];
  12,   [ (11, 5); (9, 4); (15, 4); (-1, -1) ];
  13,   [ (-1, -1); (-1, -1); (14, 4); (-1, -1) ];
  14,   [ (13, 4); (11, 1); (15, 3); (-1, -1) ];
  15,   [ (14, 3); (12, 4); (-1, -1); (-1, -1) ]
]

let next map s = 
  List.map (fun (s, _) -> s) (List.assoc s map)

let rec all map stop start = 
  let rec path accu p node = 
    if node = -1 || List.mem node p then 
      accu
    else if node = stop then 
      ((stop :: p)) :: accu
    else 
      List.fold_left (fun accu -> path accu (node :: p)) accu (next map node)
  in
  path [] [] start

let cost map path = 
  let rec aux cost c = function
    | [] -> cost
    | x :: xs -> 
	let nexts = List.assoc c map in
	let dcost = List.assoc x nexts in 
	aux (cost + dcost) x xs
  in
  aux 0 (List.hd path) (List.tl path)

let all_cost map start stop = 
  let alls = (all map start stop) in
  List.length alls, List.map (fun x -> (x, cost map x)) alls 

let gen_random_map node_coef cost_coef size = 
  let matrix = Array.create_matrix size size (true, true, true, true) in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      let left = i <> 0 && (Random.int node_coef <> 0) in
      let right = i <> size - 1 && (Random.int node_coef <> 0) in
      let up = j <> size - 1 && (Random.int node_coef <> 0) in
      let down = j <> 0 && (Random.int node_coef <> 0) in
      matrix.(i).(j) <- (left, up, right, down)
    done
  done;
  let set flag v = 
    if flag then 
      (v, 1 + Random.int cost_coef) 
    else 
      (-1, -1) 
  in
  let accu = ref [] in
  let offset i j = j * size + i in
  for j = 0 to size - 1 do 
    for i = 0 to size - 1 do
      let (left, up, right, down) = matrix.(i).(j) in
      accu := 
	(offset i j, [ set left  (offset (i - 1) j); 
		       set up    (offset i (j + 1)); 
		       set right (offset (i + 1) j); 
		       set down  (offset i (j - 1))
		     ]) :: !accu
    done
  done;
  !accu
