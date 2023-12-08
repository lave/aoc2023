open Common
open Printf

module StringSet = Set.Make(String)

let parse_node line = 
    let [node; ways] = String.split_on_char '=' line in
    let [left; right] = String.split_on_char ',' ways in
    (String.trim node, (String.trim (drop_char '(' left), String.trim (drop_char ')' right)))


let find_path_length map moves ends start =
    let rec find current n (m :: ms) =
        if StringSet.mem current ends then n
        else
            let next = (if m = 'L' then fst else snd) @@ Hashtbl.find map current in
            let ms_ = if ms = [] then moves else ms in
            find next (n + 1) ms_
    in
    find start 0 moves

let find_ghost_path_length map moves =
    let nodes = List.of_seq @@ Hashtbl.to_seq_keys map in
    let starts = List.filter (String.ends_with ~suffix:"A") nodes in
    let ends = StringSet.of_list @@ List.filter (String.ends_with ~suffix:"Z") nodes in
    (* find all individual paths - they are all looped w/o prefix (we don't check this, but it's coincidentially so) *)
    let paths_lengths = List.map (find_path_length map moves ends) starts in
    (* common path for all starting nodes will have length which is the least common divisor of all individual paths *)
    fold_left1 lcm paths_lengths


let moves_str :: nodes_strs = non_empty_strings @@ read_lines @@ input_file
let moves = to_chars moves_str
let nodes = List.map parse_node nodes_strs

let map = Hashtbl.of_seq @@ List.to_seq nodes

let path_length = find_path_length map moves (StringSet.of_list ["ZZZ"]) "AAA"
let () = printf "%d\n" path_length

let path_length = find_ghost_path_length map moves
let () = printf "%d\n" path_length
