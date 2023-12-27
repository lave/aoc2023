open Common
open Printf


let rec find_digit digits_map line : int =
    let c = String.get line 0 in
    if c >= '0' && c <= '9' then
        int_of_char c
    else
        let m = List.find_opt (fun (name, _) -> String.starts_with ~prefix:name line) digits_map in
        if Option.is_some m then
            snd @@ Option.get m
        else
            find_digit digits_map @@ Str.string_after line 1

let find_digits digits_map line : int =
    let reverse_digits_map = List.map (on_fst reverse_string) digits_map in
    let d1 = find_digit digits_map line in
    let d2 = find_digit reverse_digits_map @@ reverse_string line in
    d1 * 10 + d2


let lines = read_lines @@ input_file

let ds = sum @@ List.map (find_digits []) lines
let () = printf "%d\n" ds

let digits = [("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9)]
let ds = sum @@ List.map (find_digits digits) lines
let () = printf "%d\n" ds
