open Common
open Printf

module IntMap = Map.Make(Int)

(* add . in the end of each line to make sure we always have some chartacter after the number -
   this allows to skit the code for checking cases when number is at the end of the row *)
let lines = List.map (fun line -> to_chars (line ^ ".")) @@ read_lines input_file
let h = List.length lines
let w = List.length @@ List.hd lines

let lines_array = Array.of_list @@ List.map Array.of_list lines

(* intended to be used with Map.update *)
let add_to_list n = fun old_value -> if Option.is_none old_value
    then Option.some [n]
    else Option.some (n :: Option.get old_value)

let is_part x y : (int * int * char) option =
    if x >= 0 && x < w && y >= 0 && y < h
        then
            let c = lines_array.(y).(x) in
            let result = c <> '.' && not (c >= '0' && c <= '9') in
            (*let () = printf "Checking %d,%d = %c = %b\n" x y c result in*)
            if result then Option.some (x, y, c) else Option.none
        else Option.none

let find_adjacent x y : (int * int * char) option =
    List.fold_left nvl Option.none [
        is_part (x-1) (y-1);
        is_part  x    (y-1);
        is_part (x+1) (y-1);
        is_part (x-1)  y;
        is_part  x     y;
        is_part (x+1)  y;
        is_part (x-1) (y+1);
        is_part  x    (y+1);
        is_part (x+1) (y+1);
    ]

let scan_position (found, x, y, n, adjacent) c =
    let x_ = x+1 in
    if c >= '0' && c <= '9' then
        (*let () = printf "Found digit %d,%d = %c\n" x y c in*)
        (found, x_, y, n * 10 + (int_of_char c), if Option.is_none adjacent then find_adjacent x y else adjacent)
    else
        if n > 0 then
            (*let () = printf "Found number %n, adjacent = %b\n" n (Option.is_some adjacent) in*)
            ((n, adjacent) :: found, x_, y, 0, Option.none)
        else
            (found, x_, y, 0, Option.none)

let scan_line (found, y, n, adjacent) line =
    let (found_, _, y_, _, _) = List.fold_left scan_position (found, 0, y, n, adjacent) line in
    (found_, y_ + 1, 0, Option.none)


(* scan lines top-to-bottom and left-to-right and find all numbers and their adjacent parts *)
let (parts, _, _, _) = List.fold_left scan_line ([], 0, 0, Option.none) lines
let parts_with_symbols = List.map (on_snd Option.get) (List.filter (Option.is_some << snd) parts)

(* calculate sum of part numbers for all parts which have adjacent symbol *)
let sum_of_part_numbers = sum (List.map fst parts_with_symbols)
let () = printf "%d\n" sum_of_part_numbers


(* find all parts adjacent to gears *)
let parts_for_gears = List.filter ((=) '*' << trd3 << snd) parts_with_symbols

(* group parts by adjacent gear *)
let parts_grouped_by_gear = List.fold_left (fun acc (n, (x, y, _)) -> IntMap.update (y * w + x) (add_to_list n) acc) IntMap.empty parts_for_gears

(* select groups which have exactly two parts (adjacent to the same gear) *)
let gears_with_two_parts = List.filter ((=) 2 << List.length) (List.map snd (IntMap.to_list parts_grouped_by_gear))

(* calculate sum of gear ratios *)
let sum_of_gear_ratios = sum (List.map product gears_with_two_parts)
let () = printf "%d\n" sum_of_gear_ratios
