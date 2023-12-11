open Common
open Printf

let find_galaxies y line =
    let chars = to_chars line in
    let galaxies = List.mapi (fun x c -> if c = '#' then Some (x, y) else None) chars in
    List.map Option.get @@ List.filter Option.is_some galaxies

let expand_galaxy empty_scale empty_rows empty_columns (x, y) =
    let empty_rows_before = List.length @@ List.filter ((>) y) empty_rows in
    let empty_columns_before = List.length @@ List.filter ((>) x) empty_columns in
    (x + empty_columns_before * (empty_scale - 1), y + empty_rows_before * (empty_scale - 1))

let dist (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)

let find_distances galaxies =
    let rec find galaxies s =
        if List.is_empty galaxies
            then s
            else
                let g :: gs = galaxies in
                let s_ = sum @@ List.map (dist g) gs in
                find gs (s + s_)
    in
    find galaxies 0


let lines = read_lines @@ input_file
let h = List.length lines
let w = String.length @@ List.hd lines

let galaxies = List.concat @@ List.mapi find_galaxies @@ lines

let empty_rows = List.filter (fun y -> not @@ List.exists ((=) y << snd) galaxies) @@ List.init h Fun.id
let empty_cols = List.filter (fun x -> not @@ List.exists ((=) x << fst) galaxies) @@ List.init w Fun.id

let expanded_galaxies = List.map (expand_galaxy 2 empty_rows empty_cols) galaxies
let sum_of_distances = find_distances expanded_galaxies
let () = printf "%d\n" sum_of_distances

let expanded_galaxies = List.map (expand_galaxy 1000000 empty_rows empty_cols) galaxies
let sum_of_distances = find_distances expanded_galaxies
let () = printf "%d\n" sum_of_distances
