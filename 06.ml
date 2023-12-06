open Common
open Printf


let parse_number_series line = 
    let [_; numbers_str] = String.split_on_char ':' line in
    List.map int_of_string @@ non_empty_strings @@ String.split_on_char ' ' numbers_str

let parse_number line = 
    let [_; number_str] = String.split_on_char ':' line in
    int_of_string @@ drop_char ' ' number_str

(*
    Total distance is symmetrical regarding to charge time T' - it's:
        - 0 if T' is 0 or T (total time)
        - T-1 if T' is 1 or T-1
        - 2*T-2 if T' is 2 or T-2
        - etc


         ***
       *******
    --*********----------------- previous record
     ***********
    ---+-----+---------------------------------------------->
    0  T'    T"  T

    So we just need to find the first time T' when resulting distance is more than the current record, and then it's
    easy to calculate the last time T" and the range length.
*)
let find_amount_of_ways_to_win (time, distance) =
    let rec find t =
        if (time - t) * t > distance then t
        else find (t + 1)
    in
    let t0 = find 0 in
    let t1 = time - t0 in
    t1 - t0 + 1


let [times_line; distances_line] = read_lines @@ input_file

let races = List.combine (parse_number_series times_line) (parse_number_series distances_line)
let amount_of_ways_to_win = product @@ List.map find_amount_of_ways_to_win races
let () = printf "%d\n" amount_of_ways_to_win

let race = (parse_number times_line, parse_number distances_line)
let amount_of_ways_to_win = find_amount_of_ways_to_win race
let () = printf "%d\n" amount_of_ways_to_win
