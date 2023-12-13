open Common
open Printf

let parse_pattern lines =
    let h = List.length lines in
    let w = String.length (List.hd lines) in
    let map = Array.of_list @@ List.map (Array.of_list << to_chars) lines in
    (map, w, h)

let print_pattern (map, w, h) =
    let () = printf "%d x %x\n" w h in
    Array.iter (fun l -> printf "%s\n" @@ string_of_chars @@ Array.to_list l) map

let transpose (map, w, h) =
    (Array.of_list @@ List.init w (fun y -> Array.of_list @@ List.init h (fun x -> map.(x).(y))), h, w)


let find_mirror_vertical smudges_count (map, w, h) =
    let rec find c =
        (*let () = printf "checking for mirror between columns %d:%d\n" c (c + 1) in*)
        let rec scan_rows y n =
            let rec scan_row d n =
                let xl = c - d in
                let xr = c + d + 1 in
                (*let () = printf "checking (%d,%d) and (%d,%d), smudge=%d\n" xl y xr y n in*)
                if xl < 0 || xr >= w then
                    (* if we reached any of the borders - consider the row matching *)
                    (true, n)
                else
                    (* compare two points *)
                    if map.(y).(xl) = map.(y).(xr) then
                        (* if points match - continue scanning the row checking points farer away from the mirror *)
                        scan_row (d + 1) n
                    else if n < smudges_count then
                        (* if points don't match, but we have some unused smudges - use one smudge and continue scanning the row *)
                        scan_row (d + 1) (n + 1)
                    else
                        (* if no smudges left - stop scanning *)
                        (false, n)
            in

            if y = h then
                (* if we scanned whole pattern from top to bottom - consider matching only if we used certain amount of smudges *)
                n = smudges_count
            else
                (* scan current row - start from the supposed mirror line *)
                let (matches, n_) = scan_row 0 n in
                (* if current row matches - scan next row, otherwise stop scanning *)
                if matches then
                    scan_rows (y + 1) n_
                else
                    false
        in

        if c = w - 1 then
            None
        else if scan_rows 0 0 then
            Some (c + 1)
        else
            find (c + 1)
    in

    (*let () = print_pattern (map, w, h) in*)
    let note = find 0 in
    (*let () = if Option.is_some note then printf "vertical mirror %d\n" @@ Option.get note else () in*)
    note


let find_mirror_horizontal smudges_count pattern = 
    Option.map (( * ) 100) @@ find_mirror_vertical smudges_count @@ transpose pattern

let find_mirror smudges_count pattern =
    let col = find_mirror_vertical smudges_count pattern in
    let row = find_mirror_horizontal smudges_count pattern in
    (* there must be exactly one result *)
    let [result] = List.map Option.get @@ List.filter Option.is_some [row; col] in
    result


let patterns = List.map parse_pattern @@ split_by ((=) String.empty) @@ read_lines @@ input_file

let sum_of_notes = sum @@ List.map (find_mirror 0) patterns
let () = printf "%d\n" sum_of_notes

let sum_of_notes = sum @@ List.map (find_mirror 1) patterns
let () = printf "%d\n" sum_of_notes
