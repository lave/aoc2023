open Common
open Printf

module IntSet = Set.Make(Int)


let parse_city lines =
    let h = List.length lines in
    let w = String.length (List.hd lines) in
    let map = Array.of_list @@ List.map (Array.of_list << (List.map (fun c -> Char.code c - Char.code '0')) << to_chars) lines in
    (map, w, h)


type direction =
    | Up
    | Down
    | Left
    | Right

let code_str = function
    | Up    -> "up"
    | Down  -> "down"
    | Left  -> "left"
    | Right -> "right"

let code (x, y, d, l) =
    let d_ = match d with
        | Up    -> 0
        | Down  -> 1
        | Left  -> 2
        | Right -> 3
    in
    (y*1000 + x)*16 + d_ * 4 + l

let straight (x, y, d, l) =
    match d with
        | Up    -> (x,     y - 1, Up,    l + 1)
        | Down  -> (x,     y + 1, Down,  l + 1)
        | Left  -> (x - 1, y,     Left,  l + 1)
        | Right -> (x + 1, y,     Right, l + 1)

let turn_left (x, y, d, _) =
    match d with
        | Up    -> (x - 1, y,     Left,  1)
        | Down  -> (x + 1, y,     Right, 1)
        | Left  -> (x,     y + 1, Down,  1)
        | Right -> (x,     y - 1, Up,    1)

let turn_right (x, y, d, _) =
    match d with
        | Up    -> (x + 1, y,     Right, 1)
        | Down  -> (x - 1, y,     Left,  1)
        | Left  -> (x,     y - 1, Up,    1)
        | Right -> (x,     y + 1, Down,  1)

let find_path_ (map, w, h) p0 =
    let rec find p visited r =
        let (x, y, d, l) = p in
        (*let () = printf "At pos %d, %d moving to %s (%d), loss %d\n" x y (code_str d) l r in*)
        if x < 0 || x >= w || y < 0 || y >= h then
            None
        else
            let cod = code p in
            if IntSet.mem cod visited then
                (*let () = printf "already visited\n" in*)
                None
            else
                let r_ = r + map.(y).(x) in
                if x = w-1 && y = h-1 then
                    Some r_
                else
                    let visited_ = IntSet.add cod visited in
                    let moves = [turn_left p; turn_right p] in
                    let moves_ = if l < 3 then
                        (straight p) :: moves
                    else
                        moves
                    in
                    let results = List.map (fun p_ -> find p_ visited_ r_) moves_ in
                    let rs = List.map Option.get @@ List.filter Option.is_some results in
                    if rs = [] then
                        None 
                    else
                        Option.some @@ minimum rs
    in
    Option.get @@ find p0 IntSet.empty 0


let find_path (map, w, h) p0 =
    let move p =
        let (x, y, d, l) = p in
        (*let () = printf "can move %d, %d, %s, %d\n" x y (code_str d) l in*)
        let moves = [turn_left p; turn_right p] in
        let moves_ = if l < 3 then
            (straight p) :: moves
        else
            moves
        in
        let moves__ = List.filter (fun (x_, y_, _, _) -> x_ >= 0 && x_ < w && y_ >= 0 && y_ < h) moves_ in
        (*let () = printf "valid moves: %d\n" @@ List.length moves__ in*)
        List.map (fun p -> let (x_, y_, _, _) = p in (p, map.(y_).(x_) - 1)) moves__
    in

    let rec find frontier visited r =
        let (can_move, cant_move) = List.partition (fun x -> (snd x) = 0) frontier in
        let () = printf "frontier %d, visited %d, r=%d, can  move %d\n" (List.length frontier) (IntSet.cardinal visited) r (List.length can_move) in
        let end_ = List.find_opt (fun ((x, y, _, _), _) -> x = w-1 && y = h-1) can_move in
        if Option.is_some end_ then
            r
        else
            let moved = List.concat @@ List.map (fun (p, _) -> move p) can_move in
            (*let moved_ = List.filter (fun x -> not @@ IntSet.mem (code x) visited) moved in*)
            let cant_move_ = List.map (fun (p, w) -> (p, w-1)) cant_move in
            let l1 = List.sort_uniq (fun x y -> compare (fst x) (fst y)) moved in
            let l2 = cant_move_ in (* already sorted *)
            let frontier_ = List.merge (fun x y -> compare (fst x) (fst y)) l1 l2 in
            let visited_ = List.fold_left (fun acc x -> IntSet.add (code (fst x)) acc) visited can_move in
            find frontier_ visited_ (r+1)
    in
    find [(p0, 0)] IntSet.empty 0

let city = parse_city @@ read_lines @@ input_file
let heat_loss = find_path city (0, 0, Right, 0)
let () = printf "%d\n" heat_loss
