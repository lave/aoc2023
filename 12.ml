open Common
open Printf

let parse_spring_row line = 
    let [springs; numbers] = String.split_on_char ' ' line in
    (to_chars springs, List.map int_of_string @@ String.split_on_char ',' numbers)

let find_arrangements_count memoize (springs, lengths) =
    let memo = Hashtbl.create 1000 in
    (*
    let () = printf "%s\n" @@ string_of_chars springs in
    let () = print_ints "" lengths in
    *)

    let rec find cs ls in_bad_group arrangement =
        (*
        let () = printf "step\n" in
        let () = print_chars "" cs in
        let () = print_ints "" lengths in
        *)

        let memoized = if memoize then Hashtbl.find_opt memo (cs, ls) else None in

        if Option.is_some memoized then
            Option.get memoized
        else
            let n =
                if List.is_empty cs then
                    (* no springs left - check whether we have any incomplete groups *)
                    if (if in_bad_group then ls = [0] else List.is_empty ls) then
                        (* no springs left, and there's no incomplete bad groups - it's a valid arrangement, return 1 to count it *)
                        (*let () = printf "OK: %s\n" (string_of_chars @@ List.rev arrangement) in*)
                        1
                    else
                        (* no springs left, but there's some incomplete bad groups - impossible *)
                        0
                else
                    let c :: cs_ = cs in
                    (* if current spring is good *)
                    if c = '.' then
                        (* if we're inside or after bad group - check whether it's complete or not *)
                        if in_bad_group then
                            let l :: ls_ = ls in
                            if l = 0 then
                                (* we're right after complete bad group - valid situation, proceed *)
                                find cs_ ls_ false (c :: arrangement)
                            else
                                (* we're inside incomplete group - invalid situation, stop *)
                                0
                        else
                            (* we're not inside the bad group - just skip this good spring and proceed further *)
                            find cs_ ls false (c :: arrangement)
                    (* if current spring is bad *)
                    else if c = '#' then
                        if ls = [] then
                            (* there's no more bad groups left - invalid situation, stop *)
                            0
                        else
                            let l :: ls_ = ls in
                            if in_bad_group && l = 0 then
                                (* we're after bad group - it should have ended here but it hadn't - invalid situation, stop *)
                                0
                            else
                                (* we're either in the middle of the bad group, or hasn't started it yet - start/continue it, decrementing its length *)
                                find cs_ (l - 1 :: ls_) true (c :: arrangement)
                    else if c = '?' then
                            (* try to treat '?' as '.' or '#' and sum both outcomes *)
                            let n1 = find ('.' :: cs_) ls in_bad_group arrangement in
                            let n2 = find ('#' :: cs_) ls in_bad_group arrangement in
                            n1 + n2
                    else failwith "Invalid character"
                in

            (*let () = printf "result =%d\n" n in*)
            let () = if memoize then Hashtbl.add memo (cs, ls) n else () in
            n
    in
    find springs lengths false []



let unfold n (springs, lengths) =
    let springs_ = repeat ((@) springs << List.cons '?') springs n in
    let lengths_ = repeat ((@) lengths) lengths n in
    (springs_, lengths_)


let spring_rows = List.map parse_spring_row @@ read_lines @@ input_file

let sum_of_arrangements = sum @@ List.map (find_arrangements_count false) spring_rows
let () = printf "%d\n" sum_of_arrangements

let unfolded_spring_rows = List.map (unfold 5) spring_rows
let sum_of_arrangements = sum @@ List.map (find_arrangements_count true) unfolded_spring_rows
let () = printf "%d\n" sum_of_arrangements
