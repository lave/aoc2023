open Common
open Printf

let parse_move line =
    let [s1; s2; _] = String.split_on_char ' ' line in
    (List.hd @@ to_chars s1, int_of_string s2)

let parse_move_ line =
    let [_; _; s3] = String.split_on_char ' ' line in
    let dir_s = List.hd @@ to_chars @@ String.sub s3 7 1 in 
    let dir = match dir_s with
        | '0' -> 'R'
        | '1' -> 'D'
        | '2' -> 'L'
        | '3' -> 'U'
    in
    let len_s = String.sub s3 2 5 in
    let len = int_of_string @@ "0x" ^ len_s in
    (dir, len)

let print_trench map =
    Array.iter (fun l -> printf "%s\n" @@ string_of_chars @@ Array.to_list l) map

let dig moves =
    let rec dig__ (x, y, trench) d l =
        if l = 0 then (x, y, trench)
        else
            let (x_, y_) = match d with
                | 'U' -> (x, y-1)
                | 'D' -> (x, y+1)
                | 'L' -> (x-1, y)
                | 'R' -> (x+1, y)
            in
            dig__ (x_, y_, (x_, y_) :: trench) d (l-1)
    in

    let dig_ (x, y, trench) (d, l) =
        dig__ (x, y, trench) d l
    in

    let (_, _, trench) = List.fold_left dig_ (0, 0, []) moves in
    trench

let fill_trench trench scale =
    (*
let () = List.iter (fun (x, y) -> printf "%d,%d, " x y) trench in
    let () = printf "\n" in
    *)

    let weight x y =
        if scale = None then 1
        else
            let Some (scale_x, scale_y) = scale in
            (trd3 scale_x.(x)) * (trd3 scale_y.(y))
    in

    let (min_x, max_x, min_y, max_y) = List.fold_left
        (fun (xi, xa, yi, ya) (x, y) -> ((min xi x), (max xa x), (min yi y), (max ya y)))
        (0, 0, 0, 0)
        trench in
    let w = max_x - min_x + 1 in
    let h = max_y - min_y + 1 in
    (*let () = printf "w=%d, h=%d\n" w h in*)

    let map = Array.make_matrix h w '.' in
    let () = List.iter (fun (x, y) -> map.(y - min_y).(x - min_x) <- '#') trench in
    (*let () = print_trench map in*)

    let rec scan_line y l =
        let rec scan x y is_in on_line n =
            (*let () = printf "%d: in=%s on_line=%s n=%d\n" x (if is_in then "yes" else "no") (if on_line = None then
                "none" else if Option.get on_line then "up" else "down") n in*)
            if x = w then
                (*let () = printf "Row %d: %d\n" y n in*)
                n
            else
                let c = map.(y).(x) in
                let dn = weight x y in
                if c = '.' then
                    if on_line = None then
                        scan (x+1) y is_in on_line (n + if is_in then dn else 0)
                    else
                        let line_start_from_up = Option.get on_line in
                        let line_end_from_down = y < h-1 && map.(y+1).(x-1) = '#' in
                        let is_in_ = if line_start_from_up = line_end_from_down then not is_in else is_in in
                        scan (x+1) y is_in_ None (n + if is_in_ then dn else 0)
                else if c = '#' then
                    if on_line = None then
                        let line_start_from_up = y > 0 && map.(y-1).(x) = '#' in
                        scan (x+1) y is_in (Some line_start_from_up) (n+dn)
                    else
                        scan (x+1) y is_in on_line (n+dn)
                else
                    failwith "should never be here"
        in
        scan 0 y false None 0
    in

    let s = sum_a @@ Array.mapi scan_line map in
    s



(*
let moves = List.map parse_move @@ read_lines @@ input_file

let trench = dig moves
let l = fill_trench trench None
let () = printf "\n%d\n" l
*)

let find_points moves =
    let dig_ (x, y, trench) (d, l) =
        let (x_, y_) = match d with
            | 'U' -> (x, y-l)
            | 'D' -> (x, y+l)
            | 'L' -> (x-l, y)
            | 'R' -> (x+l, y)
        in
        (x_, y_, (x_, y_) :: trench)
    in

    let (_, _, points) = List.fold_left dig_ (0, 0, []) moves in

    (*
    let (min_x, max_x, min_y, max_y) = List.fold_left
        (fun (xi, xa, yi, ya) (x, y) -> ((min xi x), (max xa x), (min yi y), (max ya y)))
        (0, 0, 0, 0)
        points in
    let w = max_x - min_x + 1 in
    let h = max_y - min_y + 1 in
    let () = printf "w=%d, h=%d\n" w h in

    let points_ = List.map (fun (x, y) -> (x - min_x, y - min_y)) points in
    (-min_x, -min_y, points_)
*)
    (0, 0, points)

let build_scale xs =
    let rec build xs prev_x scale =
        if xs = [] then
            List.rev scale
        else
            let x :: xs_ = xs in
            if prev_x = None then
                build xs_ (Some x) @@ (x, x, 1) :: scale
            else
                let px = Option.get prev_x in
                if x - px = 1 then
                    build xs_ (Some x) @@ (x, x, 1) :: scale
                else
                    build xs_ (Some x) @@ (x, x, 1) :: (px + 1, x, x - px - 1) :: scale
    in
    build xs None []


let build_scaled_map scale_x scale_y x0 y0 moves =
    let rec dig (x, y, trench) (d, l) =
        (*let () = printf "%d,%d: %c %d\n" x y d l in*)
        if l = 0 then (x, y, trench)
        else if l < 0 then
            failwith "should never be here"
        else
            let (x_, y_, l_) = match d with
                | 'U' -> (x, y-1, l - trd3 scale_y.(y-1))
                | 'D' -> (x, y+1, l - trd3 scale_y.(y+1))
                | 'L' -> (x-1, y, l - trd3 scale_x.(x-1))
                | 'R' -> (x+1, y, l - trd3 scale_x.(x+1))
            in
            dig (x_, y_, (x_, y_) :: trench) (d, l_)
    in
    let (_, _, trench) = List.fold_left dig (x0, y0, []) moves in
    trench


let moves = List.map parse_move_ @@ read_lines @@ input_file
(*let () = printf "%c %d\n" (fst @@ List.hd moves) (snd @@ List.hd moves)*)
let (x0, y0, points) = find_points moves
(*let () = List.iter (fun (x, y) -> printf "%d,%d, " x y) points*)

let xs = List.sort_uniq compare @@ List.map fst points
(*let () = print_ints "Xs: " xs*)
let ys = List.sort_uniq compare @@ List.map snd points
(*let () = print_ints "Ys: " ys*)

let xs_ = build_scale xs
(*
let () = List.iter (fun (l, r, w) -> printf "%d-%d %d, " l r w) xs_
let () = printf "\n"
*)
let ys_ = build_scale ys
(*
let () = List.iter (fun (l, r, w) -> printf "%d-%d %d, " l r w) ys_
let () = printf "\n"
*)

let scale_x = Array.of_list xs_
let scale_y = Array.of_list ys_
let Some x0_ = Array.find_index (fun (l, r, w) -> l = 0 && r = 0) scale_x
let Some y0_ = Array.find_index (fun (l, r, w) -> l = 0 && r = 0) scale_y
let scaled_trench = build_scaled_map scale_x scale_y x0_ y0_ moves

let l = fill_trench scaled_trench @@ Option.some (scale_x, scale_y)
let () = printf "\n%d\n" l
