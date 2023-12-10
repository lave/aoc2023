open Common
open Printf

module IntSet = Set.Make(Int)

let get_adjacent (map, w, h) (x, y) =
    match map.(y).(x)  with
        | '|' -> [(x, y - 1); (x, y + 1)]
        | '-' -> [(x - 1, y); (x + 1, y)]
        | 'L' -> [(x, y - 1); (x + 1, y)]
        | 'J' -> [(x, y - 1); (x - 1, y)]
        | '7' -> [(x - 1, y); (x, y + 1)]
        | 'F' -> [(x + 1, y); (x, y + 1)]

let find_adjacent_to_start map p0 =
    let (_, w, h) = map in
    let (x, y) = p0 in
    (* consider four adjacent positions and find two of them from which one can get back to start position *)
    let adjacent = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] in
    let [p1; p2] = List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < w && y < h && List.mem p0 @@ get_adjacent map (x, y)) adjacent in
    (p1, p2)

let get_next map p_prev p =
    let adjacent = get_adjacent map p in
    let [not_previous] = List.filter ((<>) p_prev) adjacent in
    not_previous

(* follow pipe starting from initial position p0 into both directions (denoted by positions p1 and p2 adjacent to p0)
   until we meet in the farest position of the pipe *)
let find_farest_position map p0 p1 p2 =
    let rec find p1_prev p1 p2_prev p2 n =
        if p1 = p2 then n
        else
            let p1_next = get_next map p1_prev p1 in
            let p2_next = get_next map p2_prev p2 in
            find p1 p1_next p2 p2_next (n + 1)
    in
    find p0 p1 p0 p2 1


let find_start_char (x, y) (x1, y1) (x2, y2) =
    let x_min = min x1 x2 in
    let x_max = max x1 x2 in
    let y_min = min y1 y2 in
    let y_max = max y1 y2 in

    if x_min = x_max then '|'
    else if y_min = y_max then '-'
    else if y_min = y && x_min = x then 'F'
    else if y_min < y && x_min = x then 'L'
    else if y_min = y && x_min < x then '7'
    else if y_min < y && x_min < x then 'J'
    else failwith "should never be here"

let replace_char (map, w, h) (x, y) c =
    let () = Array.set map.(y) x c in
    (map, w, h)


let idx w (x, y) = y*w + x

let mark_visited set w c =
    IntSet.add (idx w c) set


let find_pipe_positions map p0 p1 =
    let w = snd3 map in
    let rec step p_prev p visited =
        if p = p0
            then visited
            else
                let p_next = get_next map p_prev p in
                step p p_next (mark_visited visited w p)
    in
    step p0 p1 @@ IntSet.singleton (idx w p0)


type turn_direction =
    | Left
    | Right

(* scan whole map line by line, starting each line from the left, and flip 'is inside' flag each time we cross the pipe -
   so that we always know whether we're inside or outside of the loop (we know that leftmost position, where we start from,
   is outside) *)
let scan_map (map, w, h) pipe =
    let rec scan_row y x n is_in boundary_start =
        (*let () = printf "scanning %d,%d, in=%b, cnt=%d\n" x y is_in n in*)
        let x_ = x + 1 in
        if x = w
            (* end of the row - return accumulated amount of positions inside pipe loop *)
            then n
        else if IntSet.mem (idx w (x, y)) pipe then
            (* this position is on the pipe - determine whether we've crossed it or not yet. Detecting crossing is trivial with
               straght vertical boundaries, but not that easy with turns - after the turn we end up being on the pipe,
               and we need to follow it until it turns agaim up or down - and whether we actually crossed it dependes on
               whether first turn and last turn were in the same direction or not *)
            match map.(y).(x) with
                | '|' -> scan_row y x_ n (not is_in) None
                | '-' -> scan_row y x_ n is_in boundary_start
                | 'L' -> scan_row y x_ n is_in (Some Left)
                | 'F' -> scan_row y x_ n is_in (Some Right)
                | '7' -> scan_row y x_ n (if boundary_start = Some Left then not is_in else is_in) None
                | 'J' -> scan_row y x_ n (if boundary_start = Some Right then not is_in else is_in) None
        else
            (* this position is not on the pipe - if it's inside, increase the counter *)
            scan_row y x_ (if is_in then n+1 else n) is_in None
    in
    sum @@ List.map (fun y -> scan_row y 0 0 false None) @@ List.init h Fun.id


let map = Array.of_list @@ List.map (Array.of_list << to_chars) @@ read_lines @@ input_file
let h = Array.length map
let w = Array.length @@ map.(0)
let map_ = (map, w, h)

(* find start position *)
let Some p0 = Array.find_mapi (fun y row -> Array.find_index ((=) 'S') row |> Option.map (fun x -> (x, y))) map

(* find positions adjacent to start position which have way to start position - there must be exactly two of them so that pipe would form a loop *)
let (p1, p2) = find_adjacent_to_start map_ p0

let farest_position = find_farest_position map_ p0 p1 p2
let () = printf "%d\n" farest_position


(* for the second part we need to replace character 'S' at the start position with actual character - find it based on
   the two adjacent positions find the proper character for the start position, and then replace in the map *)
let s = find_start_char p0 p1 p2
(* let () = printf "Start char: %c\n" s *)
let map_ = replace_char map_ p0 s


(* mark all the positions belonging to the pipe - for that walk from start position in one direction until we return back to start position *)
let pipe_positions = find_pipe_positions map_ p1 p0
let inner_positions_count = scan_map map_ pipe_positions
let () = printf "%d\n" inner_positions_count
