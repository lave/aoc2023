open Common
open Printf


let hash str =
    List.fold_left (fun h c -> (h + Char.code c) * 17 mod 256) 0 @@ to_chars str

let run_commands cmds =
    let run hashmap cmd =
        if String.ends_with ~suffix:"-" cmd then
            (* delete command *)
            let label = String.sub cmd 0 (String.length cmd - 1) in
            let idx = hash label in
            let lenses = hashmap.(idx) in
            let lenses_ = List.remove_assoc label lenses in
            let () = Array.set hashmap idx lenses_ in
            hashmap
        else
            (* put command *)
            let [label; value] = String.split_on_char '=' cmd in
            let idx = hash label in
            let lenses = hashmap.(idx) in
            let lenses_ = put_assoc label lenses @@ int_of_string value in
            let () = Array.set hashmap idx lenses_ in
            hashmap
    in
    List.fold_left run (Array.make 256 []) cmds


let cmds = String.split_on_char ',' @@ List.hd @@ read_lines @@ input_file
let sum_of_cmd_hashes = sum @@ List.map hash cmds
let () = printf "%d\n" @@ sum_of_cmd_hashes

let hashmap = run_commands cmds
let focusing_power = sum @@ Array.to_list @@ Array.mapi (fun box_number lenses ->
    (box_number + 1) * (sum @@ List.mapi (fun slot_number (_, focal_length) -> (slot_number + 1) * focal_length) lenses)) hashmap
let () = printf "%d\n" focusing_power
