open Common
open Printf


let parse_contraption lines =
    let h = List.length lines in
    let w = String.length (List.hd lines) in
    let map = Array.of_list @@ List.map (Array.of_list << to_chars) lines in
    (map, w, h)


type direction =
    | Up
    | Down
    | Left
    | Right

let code = function
    | Up    -> 1
    | Down  -> 2
    | Left  -> 4
    | Right -> 8

(* empty tile - just keep moving in the current direction *)    
let forward (x, y, d) =
    match d with
        | Up    -> [(x,     y - 1, Up   )]
        | Down  -> [(x,     y + 1, Down )]
        | Left  -> [(x - 1, y,     Left )]
        | Right -> [(x + 1, y,     Right)]

(* mirror \ - reflect accordingly *)
let reflect (x, y, d) =
    match d with
        | Up    -> [(x - 1, y,     Left )]
        | Down  -> [(x + 1, y,     Right)]
        | Left  -> [(x,     y - 1, Up   )]
        | Right -> [(x,     y + 1, Down )]

(* mirror / - reflect accordingly *)
let reflect_ (x, y, d) =
    match d with
        | Up    -> [(x + 1, y,     Right)]
        | Down  -> [(x - 1, y,     Left )]
        | Left  -> [(x,     y + 1, Down )]
        | Right -> [(x,     y - 1, Up   )]

(* splitter - - split or move in the current direction *)
let split_h (x, y, d) =
    match d with
        | Up | Down -> [(x + 1, y, Right); (x - 1, y, Left)]
        | Left      -> [(x - 1, y, Left )]
        | Right     -> [(x + 1, y, Right)]

(* splitter | - split or move in the current direction *)
let split_v (x, y, d) =
    match d with
        | Up           -> [(x, y - 1, Up  )]
        | Down         -> [(x, y + 1, Down)]
        | Left | Right -> [(x, y + 1, Down); (x, y - 1, Up)]


let get_energized_count (map, w, h) entry =
    let energized = Array.make_matrix h w 0 in

    let rec trace pos =
        let (x, y, d) = pos in
        if x < 0 || x >= w || y < 0 || y >= h then
            ()
        else
            (* we stop tracing if we already visited this tile going in the same direction - we store visited directions as a bitmap *)
            let d_code = code d in
            let energized_code = energized.(y).(x) in
            let visited = (energized_code land d_code) > 0 in

            if visited then
                (* stop tracing this path *)
                ()
            else
                (* mark this tile and direction as visited *)
                let () = Array.set (energized.(y)) x (energized_code lor d_code) in
                (* move to the next tile(s) *)
                let move_function = match map.(y).(x) with
                    | '.'  -> forward
                    | '\\' -> reflect
                    | '/'  -> reflect_
                    | '-'  -> split_h
                    | '|'  -> split_v
                in
                List.iter trace @@ move_function pos
    in
    let () = trace entry in
    (* count energized tiles *)
    sum_a @@ Array.map (sum_a << Array.map (rcompare 0)) energized


let all_beam_enters (_, w, h) =
    let tops    = List.init w (fun i -> (i,     0,     Down )) in
    let bottoms = List.init w (fun i -> (i,     h - 1, Up   )) in
    let lefts   = List.init h (fun i -> (0,     i,     Right)) in
    let rights  = List.init h (fun i -> (w - 1, i,     Left )) in
    List.concat [tops; bottoms; lefts; rights]


let contraption = parse_contraption @@ read_lines @@ input_file
let energized_count = get_energized_count contraption (0, 0, Right)
let () = printf "%d\n" energized_count

let max_energized_count = maximum @@ List.map (get_energized_count contraption) @@ all_beam_enters contraption
let () = printf "%d\n" max_energized_count
