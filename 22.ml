open Common
open Printf


let parse_point s = 
    let [x; y; z] = List.map int_of_string @@ String.split_on_char ',' s in
    (x, y, z)

let parse_brick line =
    let [e1; e2] = String.split_on_char '~' line in
    let p1 = parse_point e1 in
    let p2 = parse_point e2 in
    (p1, p2)

let is_between x x1 x2 =
    x >= (min x1 x2) && x <= (max x1 x2)

let intersect x1 x2 x3 x4 =
    is_between x1 x3 x4 || is_between x2 x3 x4 || is_between x3 x1 x2

(* check that second brick is below first brick *)
let is_below ((x1, y1, z1), (x2, y2, z2)) ((x3, y3, z3), (x4, y4, z4)) =
    if z1 = z2 then
        (* upper block is horizontal *)
        if z3 = z4 then
            (* lower block is horizontal *)
            z3 = z1-1 && (
                (* parallel along X axis *)
                (x1 = x2 && x3 = x4 && x1 = x3 && intersect y1 y2 y3 y4) ||
                (* parallel along Y axis *)
                (y1 = y2 && y3 = y4 && y1 = y3 && intersect x1 x2 x3 x4) ||
                (* cross *)
                (x1 = x2 && y3 = y4 && is_between x1 x3 x4 && is_between y3 y1 y2) ||
                (* cross *)
                (y1 = y2 && x3 = x4 && is_between y1 y3 y4 && is_between x3 x1 x2))
        else
            (* lower brick is vertical *)
            let z_ = max z3 z4 in
            z_ = z1-1 && (
                (x1 = x2 && x3 = x1 && is_between y3 y1 y2) ||
                (y1 = y2 && y3 = y1 && is_between x3 x1 x2))
    else
        (* upper brick is vertical *)
        let z = (min z1 z2) - 1 in
        if z3 = z4 then
            (* lower block is horizontal *)
            z3 = z && (
                (x3 = x4 && x3 = x1 && is_between y1 y3 y4) ||
                (y3 = y4 && y3 = y1 && is_between x1 x3 x4))
        else
            (* lower brick is vertical *)
            let z_ = max z3 z4 in
            z_ = z && x1 = x3 && y1 = y3


let settle bricks =
    let can_move bricks b = 
        let ((_, _, z1), (_, _, z2)) = b in
        z1 > 1 && z2 > 1 && List.find_opt (is_below b) bricks = None
    in

    let rec settle_ bricks =
        let can_move_ = List.map (can_move bricks) bricks in
        let moved = any can_move_ in
        if moved then
            let moved_ = List.map2 (fun b ((x1,y1,z1),(x2,y2,z2)) ->
                if b then
                    ((x1,y1,z1-1),(x2,y2,z2-1))
                else
                    ((x1,y1,z1),(x2,y2,z2))
            ) can_move_ bricks in
            settle_ moved_
        else
            bricks
    in
    settle_ bricks


let find_to_remove bricks =
    let can_be_removed b =
        let above = List.filter (fun b_ -> is_below b_ b) bricks in
        let ns = List.map (fun abov -> List.length @@ List.filter (is_below abov) bricks) above in
        all @@ List.map (fun n -> n > 1) ns
    in

    List.filter can_be_removed bricks

let would_fall_count_ bricks =
    let memo = Hashtbl.create 1000 in

    let rec cnt b =
        let memoized = Hashtbl.find_opt memo b in
        let ((x1,y1,z1),(x2,y2,z2)) = b in
        match memoized with
            | Some n -> n
            | None ->
            let () = printf "checking brick %d,%d,%d ~ %d,%d,%d\n" x1 y1 z1 x2 y2 z2 in
            let above = List.filter (fun b_ -> is_below b_ b) bricks in
            let () = printf "  %d bricks above\n" @@ List.length above in
            let ns = List.map (fun abov -> (abov, List.length @@ List.filter (is_below abov) bricks)) above in
            let would_fall = List.filter (fun (_, n) -> n = 1) ns in
            let res = List.length would_fall + (sum @@ List.map (cnt << fst) would_fall) in
            let () = Hashtbl.add memo b res in
            let () = printf "result for brick %d,%d,%d ~ %d,%d,%d is %d\n" x1 y1 z1 x2 y2 z2 res in
            res
    in

    List.map cnt bricks


let would_fall_count bricks =

    let rec cnt left removed n =
        let above = List.sort_uniq compare @@ List.concat @@ List.map (fun b -> List.filter (fun b_ -> is_below b_ b) left) removed in
        let would_fall = List.filter (fun abov -> None = List.find_opt (is_below abov) left) above in
        if would_fall = [] then
            n
        else
            let left_ = List.fold_left (Fun.flip remove) left would_fall in
            cnt left_ would_fall (n + List.length would_fall)
    in

    List.map (fun b -> cnt (remove b bricks) [b] 0) bricks




    
let bricks = List.map parse_brick @@ read_lines @@ input_file

let settled = settle bricks

let can_be_removed = find_to_remove settled
let () = printf "%d\n" @@ List.length can_be_removed

let fall_cnts = would_fall_count settled
let () = printf "%d\n" @@ sum fall_cnts
