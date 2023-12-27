open Common
open Printf

module IntSet = Set.Make(Int)


type direction =
    | Up
    | Down
    | Left
    | Right
let show_direction = function
    | Up    -> "up"
    | Down  -> "down"
    | Left  -> "left"
    | Right -> "right"


let parse_city lines =
    let h = List.length lines in
    let w = String.length (List.hd lines) in
    let map = Array.of_list @@ List.map (Array.of_list << (List.map int_of_char) << to_chars) lines in
    (map, w, h)

let idx (x, y, d, n) =
    let d_ = match d with
        | Up    -> 0
        | Down  -> 1
        | Left  -> 2
        | Right -> 3
    in
    (* x and y are less than 200, there's also 4 directions and 11 values for amount if consequtive moves in this
       direction - so these coefficients guarantee unique values for all possible coordinates *)
    ((y * 200 + x) * 4 + d_) * 11 + n

let forward (x, y, d, n) =
    match d with
        | Up    -> (x,     y - 1, Up,    n + 1)
        | Down  -> (x,     y + 1, Down,  n + 1)
        | Left  -> (x - 1, y,     Left,  n + 1)
        | Right -> (x + 1, y,     Right, n + 1)

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


let find_path_with_minimal_heat_loss (map, w, h) is_ultra_crucible =
    let (min_forward_moves, max_forward_moves) = if is_ultra_crucible then (3, 10) else (0, 3) in

    let add_all visited points =
        List.fold_left (fun acc x -> IntSet.add (idx @@ fst x) acc) visited points in

    let move p =
        let (x, y, d, n) = p in
        (*let () = printf "can move %d, %d, %s, %d\n" x y (show_direction d) n in*)
        let fwd = if n < max_forward_moves then [forward p] else [] in
        let turns = if n > min_forward_moves then [turn_left p; turn_right p] else [] in
        let moves = fwd @ turns in
        (* don't consider moves which would lead out of the map *)
        let valid_moves = List.filter (fun (x, y, _, _) -> x >= 0 && x < w && y >= 0 && y < h) moves in
        (*let () = printf "valid moves: %d\n" @@ List.length valid_moves in*)
        List.map (fun p -> let (x, y, _, _) = p in (p, map.(y).(x) - 1)) valid_moves
    in

    (* We'll consider expanding reachable area from starting point until we reach end point, but in order to make the
       process easier to comprehend we'll substitute concept of heat loss with time - each block takes certain amount of
       time to pass. So, each point in the "frontier" is coordinate (x, y, direction and amount of moves made in this
       direction), plus time we should wait before making the next move.
    *)
    let rec find frontier visited time =
        (* separate "frontier" into points which we can move now (time to go through the current block has passed), and
           points which we can't move yet (not enough time has passed to go through the block)
        *)
        let (can_move, cant_move) = List.partition (((=) 0) << snd) frontier in
        (*let () = printf "time=%d, frontier %d, visited %d, can move %d\n%!" time (List.length frontier) (IntSet.cardinal visited) (List.length can_move) in*)
        let end_block = List.find_opt (fun ((x, y, _, _), _) -> x = w - 1 && y = h - 1) can_move in
        if Option.is_some end_block then
            time
        else
            (* move points of the frontier which we can move now *)
            let moved = List.concat @@ List.map (move << fst) can_move in
            (* don't consider points which we already have visited, also remove duplicates *)
            let moved_new = List.sort_uniq compare @@ List.filter (fun (p, _) -> not @@ IntSet.mem (idx p) visited) moved in
            (* decrement remaining wait time for points which we can't move yet *)
            let cant_move_ = List.map (fun (p, w) -> (p, w - 1)) cant_move in
            let frontier_ = moved_new @ cant_move_ in
            (* add all point to which we moved to visited set *)
            let visited_ = add_all visited moved_new in
            find frontier_ visited_ (time + 1)
    in

    (* initial frontier consists of two points with different directions - right and down (this makes no difference in
       the first part because we can turn immediately, but is important in the second part, where we can't turn
       immediately )
    *)
    let frontier = [((0, 0, Right, 0), 0); ((0, 0, Down, 0), 0)] in
    let visited = add_all IntSet.empty frontier in
    find frontier visited 0


let city = parse_city @@ read_lines @@ input_file

let heat_loss = find_path_with_minimal_heat_loss city false
let () = printf "%d\n" heat_loss

let heat_loss = find_path_with_minimal_heat_loss city true
let () = printf "%d\n" heat_loss
