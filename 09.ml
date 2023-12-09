open Common
open Printf

let parse_sequence line = 
    List.map int_of_string @@ String.split_on_char ' ' line

let rec predict seq =
    let non_zero = List.find_opt ((<>) 0) seq in
    if Option.is_none non_zero
        then 0
        else 
            let rseq = List.rev seq in
            let initial = List.tl seq in
            let shifted = List.rev @@ List.tl rseq in
            let diff_seq = List.map2 (-) initial shifted in
            let next_diff = predict diff_seq in
            let next = (List.hd rseq) + next_diff in
            next

let seqs = List.map parse_sequence @@ read_lines @@ input_file

let nexts = List.map predict seqs
let () = printf "%d\n" @@ sum nexts

let reversed_seqs = List.map List.rev seqs
let nexts = List.map predict reversed_seqs
let () = printf "%d\n" @@ sum nexts
