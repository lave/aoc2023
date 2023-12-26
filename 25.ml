open Common
open Printf

module StrSet = Set.Make(String)


let parse_connections line =
    let [from; tos] = String.split_on_char ':' line in
    let tos_ = String.split_on_char ' ' @@ String.sub tos 1 @@ (String.length tos) - 1 in
    (from, tos_)

let parse_device lines =
    let connections = List.map parse_connections lines in
    let edges = List.concat @@ List.map (fun (from, tos) -> List.map (fun t -> (from, t)) tos) connections in
    edges


let build_graph g =
    let nodes = Hashtbl.create @@ List.length g in
    let () = List.iter (fun (from, to_) ->
        let n_from = Option.value ~default:[] @@ Hashtbl.find_opt nodes from in
        let n_to   = Option.value ~default:[] @@ Hashtbl.find_opt nodes to_ in
        let () = Hashtbl.replace nodes from @@ to_ :: n_from in
        let () = Hashtbl.replace nodes to_  @@ from :: n_to in
        ()
    ) g in
    nodes

let walk g n0 =
    let rec walk_ nodes visited n =
        (*
        let () = print_strs "Frontier" nodes in
        let () = print_strs "Visited" (StrSet.to_list visited) in
        let () = printf "N=%d\n" n in
        *)

        let nodes_ = List.sort_uniq compare @@ List.concat @@ List.map (fun n -> Hashtbl.find g n) nodes in
        let not_visited = List.filter (fun n -> not @@ StrSet.mem n visited) nodes_ in
        if not_visited = [] then
            n
        else
            let visited_ = List.fold_left (fun acc n -> StrSet.add n acc) visited not_visited in
            walk_ not_visited visited_ (n + List.length not_visited)
    in
    walk_ [n0] (StrSet.singleton n0) 1


let split_graph_ edges =
    let edges_ = Array.of_list edges in
    let n_e = List.length edges in
    let g = build_graph edges in
    let n_n = Hashtbl.length g in
    let start = fst @@ List.hd edges in

    let del_edges i j k =
        let del_edge x =
            let (from, to_) = edges_.(x) in
            let () = Hashtbl.add g from @@ remove to_  @@ Hashtbl.find g from in
            let () = Hashtbl.add g to_  @@ remove from @@ Hashtbl.find g to_ in
            ()
        in
        let () = del_edge i in
        let () = del_edge j in
        let () = del_edge k in
        ()
    in

    let restore_edges i j k =
        let restore_edge x =
            let (from, to_) = edges_.(x) in
            let () = Hashtbl.remove g from in
            let () = Hashtbl.remove g to_ in
            ()
        in
        let () = restore_edge k in
        let () = restore_edge j in
        let () = restore_edge i in
        ()
    in

    let is_split (i,j,k) =
        let () = if k=j+1 then printf "W/O edges %d, %d and %d\n%!" i j k else () in
        let () = del_edges i j k in
        let m = walk g start in
        let () = restore_edges i j k in
        (*let () = print_strs "nodes" @@ List.of_seq @@ Hashtbl.to_seq_keys g__ in*)
        if m = n_n then
            None
        else
            Option.some (m, n_n-m)
    in

    let rec split i j k =
        if i = n_e then
            None
        else
            let res = is_split (i,j,k) in
            if res = None then
                let (i_, j_, k_) = if j=n_e-2 && k=n_e-1 then
                    (i+1, i+2, i+3)
                else if k=n_e-1 then
                    (i, j+1, j+2)
                else
                    (i, j, k+1)
                in
                split i_ j_ k_
            else
                res
    in

    let () = printf "%d\n%!" @@ List.length edges in
    let Some res = split 0 1 2 in
    (*let () = printf "%d, %d\n" (fst res) (snd res) in*)
    res



let split_graph g =
    let rec find chosen frontier n =

        (*
        let () = print_strs "\nChosen" (StrSet.to_list chosen) in
        let () = print_strs "Frontier" frontier in
        let () = printf "N=%d\n" n in
        *)

        if n = 3 then
            let m = StrSet.cardinal chosen in
            (m, Hashtbl.length g -m)
        else
            let nexts = List.map (fun n ->
                let es = Hashtbl.find g n in
                (n, List.length @@ List.filter (fun n_ -> StrSet.mem n_ chosen) es)
            ) frontier in
            let sorted = List.sort (on snd rcompare) nexts in
            let top = List.hd sorted in
            (*let () = printf "top %s, weight %d\n" (fst top) (snd top) in*)
            let tops = List.filter (fun (_, n) -> n = snd top) sorted in
            (*let () = print_strs "tops" (List.map fst tops) in*)
            let i = Random.int (List.length tops) in
            let next = List.nth tops i in
            (*let () = printf "chose %s, weight %d\n" (fst next) (snd next) in*)
            let next_neight = Hashtbl.find g (fst next) in
            (*let () = print_strs "neightbours" next_neight in*)
            let non_chosen = List.filter (fun n_ -> not (StrSet.mem n_ chosen)) next_neight in
            (*let () = print_strs "new chosen" non_chosen in*)
            let frontier_ = List.sort_uniq compare @@ non_chosen @ remove (fst next) frontier in
            let n_ = n - (snd next) + (List.length non_chosen) in
            (*let () = printf "new N=%d\n" n_ in*)
            find (StrSet.add (fst next) chosen) frontier_ n_
    in

    let Some (start, _) = Seq.uncons @@ Hashtbl.to_seq_keys g in
    let frontier = Hashtbl.find g start in
    find (StrSet.singleton start) frontier @@ List.length frontier

let connections = parse_device @@ read_lines @@ input_file
let device = build_graph connections
(*
let () = printf "Nodes: %d\n" @@ Hashtbl.length device
let () = Hashtbl.iter (fun n es -> printf "%d %s\n" (List.length es) n) device
*)

let sizes = split_graph device
let n = (fst sizes) * (snd sizes)
let () = printf "%d\n" n
