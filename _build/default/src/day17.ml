let read_file filename =
  let in_channel = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line in_channel in
      read_lines (line :: acc)
    with End_of_file ->
      close_in in_channel;
      List.rev acc (* Reverse the list to maintain the original order *)
  in
  read_lines []

let parse_input lines =
  let register_a = ref 0 in
  let register_b = ref 0 in
  let register_c = ref 0 in
  let program = ref [] in

  List.iter
    (fun line ->
      match String.split_on_char ':' line with
      | [ "Register A"; value ] ->
          register_a := int_of_string @@ String.trim value
      | [ "Register B"; value ] ->
          register_b := int_of_string @@ String.trim value
      | [ "Register C"; value ] ->
          register_c := int_of_string @@ String.trim value
      | [ "Program"; value ] ->
          String.trim value |> String.split_on_char ','
          |> List.iter (fun value ->
                 program := int_of_string (String.trim value) :: !program)
      | _ -> ())
    lines;
  ((register_a, register_b, register_c), program)

let combo operand registers =
  let a, b, c = registers in
  match operand with
  | opr when opr >= 0 && opr <= 3 -> opr
  | 4 -> !a
  | 5 -> !b
  | 6 -> !c
  | _ -> 0

let literal lit = lit

let apply_instruction (opcode, operand) (a, b, c) pc console =
  let dv reg =
    let numerator = !a in
    let denominator =
      int_of_float (2.0 ** float_of_int (combo operand (a, b, c)))
    in
    reg := numerator / denominator
  in

  match opcode with
  | 0 -> dv a
  | 1 ->
      let xor_value = !b lxor literal operand in
      b := xor_value
  | 2 -> b := combo operand (a, b, c) mod 8
  | 3 -> if !a != 0 then pc := literal operand
  | 4 -> b := !b lxor !c
  | 5 -> console := (combo operand (a, b, c) mod 8) :: !console
  | 6 -> dv b
  | 7 -> dv c
  | _ -> failwith "invalid"

let run (a, b, c) program =
  let pc = ref 0 in
  let console = ref [] in

  program := List.rev !program;

  while !pc < List.length !program do
    let opcode = List.nth !program !pc in
    let operand = List.nth !program (!pc + 1) in
    let pc_before = !pc in
    apply_instruction (opcode, operand) (a, b, c) pc console;
    if pc_before = !pc then pc := !pc + 2
  done;

  String.concat "," (List.map string_of_int (List.rev !console))

let () =
  let (a, b, c), program = read_file "./input17.txt" |> parse_input in
  let output = run (a, b, c) program in
  Printf.printf "Result 1: %s" output
