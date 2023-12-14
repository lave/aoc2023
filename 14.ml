open Common
open Printf

let parse_platform lines =
    Array.of_list @@ List.map (Array.of_list << to_chars) lines

let print_row row =
     printf "%s\n" @@ string_of_chars @@ Array.to_list row

let print_platform platform =
    let () = Array.iter print_row platform in
    printf "\n"


let tilt_left platform =
    let rec grow_to n row =
        if List.length row < n then grow_to n ('.' :: row) else row
    in

    let tilt_row row =
        let w = Array.length row in
        (*let () = print_row row in*)
        let rec tilt x row_ =
            if x = w then
                let row__ = Array.of_list @@ List.rev @@ grow_to x row_ in
                (*let () = print_row row__ in*)
                row__
            else
                let x_ = x + 1 in
                let c = row.(x) in
                if c = 'O' then
                    (*let () = printf "rock at %d rolls to pos %d\n" x (List.length row_) in*)
                    tilt x_ (c :: row_)
                else if c = '#' then
                    (*let () = printf "resetting floor to %d\n" x in*)
                    tilt x_ (c :: (grow_to x row_))
                else
                    tilt x_ row_
        in

        tilt 0 []
    in

    (*let () = print_platform platform in*)
    let platform_ = Array.map tilt_row platform in
    (*let () = print_platform platform_ in*)
    platform_


let tilt_up = transpose << tilt_left << transpose

let tilt_down = anti_transpose << tilt_left << anti_transpose

let tilt_right = mirror_h << tilt_left << mirror_h

let cycle = tilt_right << tilt_down << tilt_left << tilt_up


let weight platform =
    let h = Array.length platform in
    let weight_row y row =
        sum @@ Array.to_list @@ Array.map (fun c -> if c = 'O' then (h - y) else 0) row
    in
    sum @@ Array.to_list @@ Array.mapi weight_row platform


(* returns pair (loop_start, loop_period) *)
let find_cycle p =
    let rec find p previous =
        let p_ = cycle p in
        (*
        let () = print_platform p_ in
        let () = printf "weight = %d\n\n" @@ weight p_ in
        *)
        (* try to find this state in the list of previous states *)
        let i = List.find_index ((=) p_) previous in
        if Option.is_none i then
            (* not found - add new state to the list of previous states and continue *)
            find p_ (p_ :: previous)
        else
            (* found - calculate loop start and period *)
            let period = Option.get i + 1 in
            let start = (List.length previous) - period in
            (start, period)
    in
    find p [p]


let platform = parse_platform @@ read_lines @@ input_file
(*let () = print_platform platform*)

let weight_after_tilt_up = weight @@ tilt_up platform
let () = printf "%d\n" weight_after_tilt_up

let (start, period) = find_cycle platform
(* calculate amount of cycles we need to make to achieve the same state on the first loop *)
let n_cycles = start + (1000000000 - start) mod period

let weight_after_many_cycles = weight @@ repeat cycle platform (n_cycles + 1)
let () = printf "%d\n" weight_after_many_cycles
