module Aocutils = struct
  let print_character_matrix matrix =
    let rowCount = Array.length matrix in
    let colCount = Array.length matrix.(0) in
    for i = 0 to rowCount - 1 do
      for j = 0 to colCount - 1 do
        Printf.printf "%c " matrix.(i).(j)
      done;
      Printf.printf "\n"
    done
end
