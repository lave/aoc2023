open Common
open Printf

let parse_numbers line : int list =
    List.map (int_of_string << String.trim) @@ non_empty_strings @@ String.split_on_char ' ' line

let parse_card line =
    let [card_id; card_content] = String.split_on_char ':' line in
    let ["Card"; id] = non_empty_strings @@ String.split_on_char ' ' card_id in
    let [winning_numbers; your_numbers] = String.split_on_char '|' card_content in
    (int_of_string id, parse_numbers winning_numbers, parse_numbers your_numbers)

let matching_numbers (_, winning, yours) =
    List.length @@ List.filter (Fun.flip List.mem winning) yours

let points card =
    let matching_count = matching_numbers card in
    if matching_count = 0 then 0 else power 2 @@ matching_count - 1

let add_cards (i, counts) card =
    let matching_count = matching_numbers card in
    let cur_count = counts.(i) in
    let counts_ = Array.mapi (fun j v -> if j > i && j <= i + matching_count then v + cur_count else v) counts in
    (i + 1, counts_)


let lines = read_lines @@ input_file
let cards = List.map parse_card lines

let sum_of_points = sum @@ List.map points cards
let () = printf "%d\n" sum_of_points

let (_, counts) = List.fold_left add_cards (0, Array.make (List.length cards) 1) cards
let sum_of_cards = sum_a counts
let () = printf "%d\n" sum_of_cards
