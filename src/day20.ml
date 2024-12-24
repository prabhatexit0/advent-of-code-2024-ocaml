open Aocutils

let parse_input grid =
  let n = List.length grid in
  let m = String.length @@ List.hd grid in
  let matrix = Array.make_matrix n m ' ' in
  List.iteri
    (fun i line -> String.iteri (fun j ch -> matrix.(i).(j) <- ch) line)
    grid;
  matrix

let is_valid_location matrix x y =
  x >= 0 && y >= 0
  && x < Array.length matrix
  && y < Array.length matrix.(0)
  && if matrix.(x).(y) != '#' then true else false

let find_distance_till_target matrix start target =
  let n = Array.length matrix in
  let m = Array.length matrix.(0) in

  let distance_matrix = Array.make_matrix n m max_int in
  distance_matrix.(fst start).(snd start) <- 0;

  let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in
  let q = ref (Queue.create ()) in
  Queue.add start !q;
  let visited = Hashtbl.create 100 in
  let found = ref false in
  let level = ref 1 in

  while not (Queue.is_empty !q) do
    let queue_size = ref (Queue.length !q) in

    for _ = 1 to !queue_size do
      let x, y = Queue.take !q in
      Hashtbl.add visited (x, y) true;

      List.iter
        (fun (dx, dy) ->
          let a, b = (x + dx, y + dy) in
          if is_valid_location matrix a b && not (Hashtbl.mem visited (a, b))
          then (
            Queue.add (a, b) !q;
            distance_matrix.(a).(b) <- !level;
            if (a, b) = target then found := true))
        directions
    done;

    level := !level + 1
  done;
  (distance_matrix.(fst target).(snd target), distance_matrix)

let find_shortest_path matrix distance_matrix start target =
  let stack = ref (Stack.create ()) in
  let visited = Hashtbl.create 100 in
  let directions = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in
  let path = ref [] in
  Stack.push start !stack;

  while not (Stack.is_empty !stack) do
    let x, y = Stack.pop !stack in
    path := (x, y) :: !path;

    if not (Hashtbl.mem visited (x, y)) then (
      Hashtbl.add visited (x, y) true;
      List.iter
        (fun (dx, dy) ->
          let a, b = (x + dx, y + dy) in
          if is_valid_location matrix a b then
            if
              is_valid_location matrix a b
              && distance_matrix.(a).(b) = distance_matrix.(x).(y) + 1
            then Stack.push (a, b) !stack)
        directions)
  done;

  if not (List.hd !path = target) then
    failwith "Something is super wrong, last element is not target";

  List.rev !path

let find_start_target matrix =
  let start, target = (ref None, ref None) in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j ch ->
          if ch = 'S' then start := Some (i, j)
          else if ch = 'E' then target := Some (i, j))
        row)
    matrix;

  (Option.value ~default:(0, 0) !start, Option.value ~default:(0, 0) !target)

(* TODO (SUPER SLOW): Optimize it*)
let get_cheat_count matrix distance_matrix path target =
  let cheat_count = ref 0 in
  let directions =
    [ (1, 1); (1, -1); (-1, 1); (-1, -1); (2, 0); (0, 2); (-2, 0); (0, -2) ]
  in
  List.iter
    (fun (x, y) ->
      List.iter
        (fun (dx, dy) ->
          let a, b = (x + dx, y + dy) in
          if is_valid_location matrix a b then
            let dist_till_now = distance_matrix.(x).(y) in
            let dist, _ = find_distance_till_target matrix (a, b) target in
            let new_distance = dist + dist_till_now + 2 in
            if distance_matrix.(fst target).(snd target) - new_distance >= 100
            then cheat_count := !cheat_count + 1)
        directions)
    path;
  !cheat_count

let () =
  let matrix = Aocutils.read_file "./inputs/20.txt" |> parse_input in
  let start, target = find_start_target matrix in
  let _, distance_matrix = find_distance_till_target matrix start target in
  let path = find_shortest_path matrix distance_matrix start target in
  let cheat_count = get_cheat_count matrix distance_matrix path target in
  Printf.printf "Result 1: %d\n" cheat_count
