open Common
open Printf

let parse_cubes line : (string * int) =
    let [number; color] = String.split_on_char ' ' (String.trim line) in
    (color, int_of_string number)

let parse_draw line : (int * int * int) =
    let cubes = List.map parse_cubes (String.split_on_char ',' line) in
    let r = Option.value ~default:0 @@ Option.map snd @@ List.find_opt ((=) "red" << fst) cubes in
    let g = Option.value ~default:0 @@ Option.map snd @@ List.find_opt ((=) "green" << fst) cubes in
    let b = Option.value ~default:0 @@ Option.map snd @@ List.find_opt ((=) "blue" << fst) cubes in
    (r, g, b)

let parse_game line =
    let [game_id; game_definition] = String.split_on_char ':' line in
    let ["Game"; id] = String.split_on_char ' ' game_id in
    let draws = List.map parse_draw @@ String.split_on_char ';' game_definition in
    (int_of_string id, draws)

let is_possible_game (max_r, max_g, max_b) (_, draws) =
    let is_possible_draw (r, g, b) = r <= max_r && g <= max_g && b <= max_b in
    all (List.map is_possible_draw draws)

let game_power (_, draws) =
    let r = maximum @@ List.map fst3 draws in
    let g = maximum @@ List.map snd3 draws in
    let b = maximum @@ List.map trd3 draws in
    r * g * b

let lines = read_lines @@ input_file
let games = List.map parse_game lines

let possible_games = List.filter (is_possible_game (12, 13, 14)) games
let sum_of_possible_games_ids = sum @@ List.map fst possible_games
let () = printf "%d\n" sum_of_possible_games_ids

let sum_of_game_powers = sum @@ List.map game_power games
let () = printf "%d\n" sum_of_game_powers
