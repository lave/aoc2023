open Common
open Printf

module IntSet = Set.Make(Int)


let parse_map lines =
    let h = List.length lines in
    let w = String.length (List.hd lines) in
    let map = Array.of_list @@ List.map (Array.of_list << to_chars) lines in
    (map, w, h)

let idx w x y = y*w+x

let find_path (map, w, h) p0 pe =

    let moves (x,y) =
        (*let () = printf "%d,%d -> %c\n" x y @@ map.(y).(x) in*)
        match map.(y).(x) with
            | '.' -> List.filter (fun (x_,y_) -> y_ > 0 && map.(y_).(x_) != '#' ) [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
            | '>' -> [(x+1,y)]
            | '<' -> [(x-1,y)]
            | '^' -> [(x,y-1)]
            | 'v' -> [(x,y+1)]
    in

    let rec find p n visited m =
        let (x,y) = p in
        if y = h-1 then
            let () = printf "%d    %d\n%!" n m in
            n
        else
            let moves_ = moves p in
            (*let () = printf "Moves for %d,%d: %n\n" x y @@ List.length moves_ in*)
            let moves__ = List.filter (
                fun (x_,y_) -> None = IntSet.find_opt (idx w x_ y_) visited
            ) @@ moves_ in
            if moves__ = [] then
                0
            else
                let res = List.map (fun p_ -> find p_ (n+1) (IntSet.add (idx w x y) visited) (m*List.length moves__)) moves__ in
                maximum res
    in
    find p0 0 IntSet.empty 1



let find_longest_path (map, w, h) p0 pe =
    let (_,ye) = pe in

    let moves (x,y) =
        List.filter (fun (x_,y_) -> y_ >= 0 && y_ < h && map.(y_).(x_) != '#' ) [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    in

    let rec advance adv non_adv =
        if non_adv = [] then
            adv
        else
            let path :: non_adv_ = non_adv in
            let (p, visited) = path in
            let (x,y) = p in
            (*let () = printf "%d,%d\n" x y in*)
            (* new moves for this path *)
            let moves_ = List.filter (
                fun (x_,y_) -> None = IntSet.find_opt (idx w x_ y_) visited
            ) @@ moves p in
            (* find intersections with other paths - remove other advanced paths which intersect this one (i.e. contain
               one of the newly found points), they took
               shorter path to reach this point so they are shorter overall *)
            let adv_ = List.filter (fun (_, visited_) -> 
                None = List.find_opt (fun (x_, y_) ->
                    None != IntSet.find_opt (idx w x_ y_) visited_
                ) moves_
            ) adv in
            let non_adv__ = List.filter (fun (_, visited_) -> 
                None = List.find_opt (fun (x_, y_) ->
                    None != IntSet.find_opt (idx w x_ y_) visited_
                ) moves_
            ) non_adv_ in
            let new_paths = List.map (fun p -> 
                (p, (IntSet.add (idx w x y) visited))) moves_ in
            advance (new_paths @ adv_) non_adv__
    in

    let rec find n paths n_max =
        let None = List.find_opt (fun (_, visited) -> IntSet.cardinal visited != n) paths in
        let (finished, unfinished) = List.partition (fun ((_, y), _) -> y = ye) paths in
        (*
        let () = List.iter (fun (_, vis) ->
            printf "Finished: %d\n" @@ IntSet.cardinal vis
        ) finished in
        let () = printf "  Unfinshed: %d\n" @@ List.length unfinished in
    *)
        let n_max_ = if finished = [] then n_max else n in
        if unfinished = [] then
            (* last finished path is the longest *)
            n_max
        else
            let paths_ = advance [] unfinished in
            find (n+1) paths_ n_max_
    in
    find 0 [(p0, IntSet.empty)] 0


let make_graph (map, w, h) =
    let edges_p x y =
        if map.(y).(x) = '#' then
            []
        else
            let dsts = List.filter (fun (x_,y_) -> y_ >= 0 && y_ < h && map.(y_).(x_) != '#' ) [(x-1,y);(x+1,y);(x,y-1);(x,y+1)] in
            List.map (fun (x_, y_) -> (idx w x y, idx w x_ y_, 1)) dsts
    in

    let edges_row y =
        List.concat @@ List.map (fun x -> edges_p x y) @@ List.init w Fun.id
    in

    let edges = List.filter (fun (n1, n2, l) -> n1 < n2) @@ List.concat @@ List.map (fun y -> edges_row y) @@ List.init h Fun.id in
    let () = printf "Edges: %d\n" @@ List.length edges in

    let nodes = Hashtbl.create (w*h) in
    let () = List.iter (fun (n1, n2, l) ->
        let n1_edges = Option.value ~default:[] @@ Hashtbl.find_opt nodes n1 in
        let n2_edges = Option.value ~default:[] @@ Hashtbl.find_opt nodes n2 in
        let () = Hashtbl.replace nodes n1 @@ (n2, l) :: n1_edges in
        let () = Hashtbl.replace nodes n2 @@ (n1, l) :: n2_edges in
        ()
    ) edges in
    let () = printf "Nodes: %d\n" @@ Hashtbl.length nodes in

    let rec simplify_graph nodes =
        let simple = Seq.find (fun (_, edges) -> 2 = List.length edges) @@ Hashtbl.to_seq nodes in
        if simple = None then
            nodes
        else
            let Some (n, [(n1, l1); (n2, l2)]) = simple in
            let () = Hashtbl.remove nodes n in
            let n1_edges = List.filter (fun (n_, _) -> n_ != n ) @@ Hashtbl.find nodes n1 in
            let n2_edges = List.filter (fun (n_, _) -> n_ != n ) @@ Hashtbl.find nodes n2 in
            let () = Hashtbl.replace nodes n1 @@ (n2, l1+l2) :: n1_edges in
            let () = Hashtbl.replace nodes n2 @@ (n1, l1+l2) :: n2_edges in
            simplify_graph nodes
    in

    let nodes_ = simplify_graph nodes in
    let () = printf "Nodes: %d\n" @@ Hashtbl.length nodes_ in

    let edges_ = List.filter (fun (n1, n2, _) -> n1 < n2) @@ List.concat @@ List.map (fun (n1, edges) -> List.map (fun (n2, l) -> (n1, n2, l)) edges) @@ List.of_seq @@ Hashtbl.to_seq nodes_ in
    let () = List.iter (fun (n1, n2, l) -> printf "%d -> %d : %d\n" n1 n2 l) edges_ in
    edges_


let find_longest g ns ne =
    let () = printf "finding path from %d to %d\n%!" ns ne in
    let rec find n visited l =
        if n = ne then
            l
        else
            let next = List.filter_map (fun (n1, n2, l) ->
                if n = n1 then Some (n2, l)
                else if n = n2 then Some (n1, l)
                else None
            ) g in
            let next_ = List.filter (fun (n1, _) -> None = IntSet.find_opt n1 visited) next in
            if next_ = [] then
                0
            else
                let res = List.map (fun (n1, l1) -> find n1 (IntSet.add n1 visited) (l+l1)) next_ in
                maximum res
    in
    find ns IntSet.empty 0




let the_map = parse_map @@ read_lines @@ input_file
let (_, w, h) = the_map
(*
let n = find_path the_map (1,0) (w-2,h-1)
let () = printf "%d\n" n
*)

(*
let n = find_longest_path the_map (1,0) (w-2,h-1)
let () = printf "%d\n" n
*)

(*
let n = find_longest_path the_map  (w-2,h-1) (1,0)
let () = printf "%d\n" n
*)
(* 4646 - low *)
(* 5154 - low *)
let g = make_graph the_map
let n = find_longest g 1 (idx w (w-2) (h-1))
let () = printf "%d\n" n
