let read_file filename =
  let input_channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line input_channel in
      read_lines (line :: acc)
    with End_of_file ->
      close_in input_channel;
      List.rev acc
  in
  read_lines []

let printCostMatrix costMatrix =
  let rowCount = Array.length costMatrix in
  let colCount = Array.length costMatrix.(0) in
  for i = 0 to rowCount - 1 do
    for j = 0 to colCount - 1 do
      let cost = costMatrix.(i).(j) in
      if cost = max_int then Printf.printf "Inf\t"
      else Printf.printf "%d \t" cost
    done;
    Printf.printf "\n"
  done

let linesToMatrix grid =
  let n = List.length grid in
  let m = String.length @@ List.hd grid in
  let matrix = Array.make_matrix n m ' ' in
  List.iteri
    (fun i line -> String.iteri (fun j ch -> matrix.(i).(j) <- ch) line)
    grid;
  matrix

type direction = U | D | L | R

module CoordCost = struct
  type t = (int * int) * int * direction

  let compare ((x1, y1), cost1, _) ((x2, y2), cost2, _) =
    match compare cost1 cost2 with
    | 0 -> ( match compare x1 x2 with 0 -> compare y1 y2 | c -> c)
    | c -> c
end

module MinHeap = Set.Make (CoordCost)

let isValidLocation a b matrix =
  a >= 0
  && a < Array.length matrix
  && b >= 0
  && b < Array.length matrix.(0)
  && matrix.(a).(b) <> '#'
  && matrix.(a).(b) <> '@'

let findLeastCost matrix start target =
  let rowCount = Array.length matrix in
  let colCount = Array.length matrix.(0) in

  let costMatrix = Array.make_matrix rowCount colCount max_int in
  costMatrix.(fst start).(snd start) <- 0;

  let directions = [ ((0, 1), R); ((0, -1), L); ((1, 0), D); ((-1, 0), U) ] in
  let q = ref MinHeap.(empty |> add (start, 0, R)) in

  while not (MinHeap.is_empty !q) do
    let (x, y), cost, d = MinHeap.min_elt !q in
    q := MinHeap.remove ((x, y), cost, d) !q;

    List.iter
      (fun ((dx, dy), nd) ->
        let dcost = if d = nd then 1 else 1001 in
        let newCost = costMatrix.(x).(y) + dcost in
        if
          isValidLocation (x + dx) (y + dy) matrix
          && newCost <= costMatrix.(x + dx).(y + dy)
        then (
          q := MinHeap.add ((x + dx, y + dy), newCost, nd) !q;
          costMatrix.(x + dx).(y + dy) <- newCost))
      directions
  done;
  (costMatrix.(fst target).(snd target), costMatrix)

let findStartTarget matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  let start = ref None in
  let end_pos = ref None in

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      match matrix.(i).(j) with
      | 'S' -> start := Some (i, j)
      | 'E' -> end_pos := Some (i, j)
      | _ -> ()
    done
  done;
  (!start, !end_pos)

let () =
  let stringList = read_file "./inputs/16.txt" in
  let matrix = linesToMatrix stringList in
  let start, target = findStartTarget matrix in
  let start_pos = Option.value ~default:(0, 0) start in
  let target_pos = Option.value ~default:(0, 0) target in
  let cost, _ = findLeastCost matrix start_pos target_pos in
  Printf.printf "Result 1: %d\n" cost
