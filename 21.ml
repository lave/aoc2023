open Common
open Printf

module IntSet = Set.Make(Int)


let parse_map lines =
    let h = List.length lines in
    let w = String.length (List.hd lines) in
    let map = Array.of_list @@ List.map (Array.of_list << to_chars) lines in
    (map, w, h)

let find_start (map, w, h) =
    Option.get @@ Array.find_mapi (fun y row -> Option.map (fun x -> (x, y)) @@ Array.find_index (fun c -> c = 'S') row) map

let idx w (x, y) = y*w + x
let un_idx w i = (i mod w, i / w)

let walk (map, w, h) p0 n_steps =

    let get_ps (x, y) =
        let ps = [(x-1,y); (x+1,y); (x, y-1); (x,y+1)] in
        let ps_ = List.filter (fun (x,y) -> x>=0 && x <w && y>=0 && y<h && map.(y).(x) != '#') ps in
        (*let () = printf "%d,%d -> %d points\n" x y (List.length ps_) in*)
        ps_
    in

    let make_step ps i =
        let ps_ = List.fold_left (fun acc p ->
            let r = List.fold_left (fun acc_ p_ ->
                let r1 = IntSet.add (idx w p_) acc_ in
                r1
                ) acc @@ get_ps p in
            r
        ) IntSet.empty ps in
        let ps__ = List.map (un_idx w) @@ IntSet.elements ps_ in
        (*
        let () = List.iter (fun (x,y) -> printf "%d,%d; " x y) ps__ in
        let () = printf "\n" in
        *)
        ps__
    in
    
    let steps = List.fold_left make_step [p0] @@ List.init n_steps Fun.id in
    List.length steps


let find_cycle (map, w, h) p0 n_steps =

    let get_ps (x, y) =
        let ps = [(x-1,y); (x+1,y); (x, y-1); (x,y+1)] in
        let ps_ = List.filter (fun (x,y) -> x>=0 && x <w && y>=0 && y<h && map.(y).(x) != '#') ps in
        (*let () = printf "%d,%d -> %d points\n" x y (List.length ps_) in*)
        ps_
    in

    let make_step ps i =
        let ps_ = List.fold_left (fun acc p ->
            let r = List.fold_left (fun acc_ p_ ->
                let r1 = IntSet.add (idx w p_) acc_ in
                r1
                ) acc @@ get_ps p in
            r
        ) IntSet.empty ps in
        let ps__ = List.map (un_idx w) @@ IntSet.elements ps_ in
        (*
        let () = List.iter (fun (x,y) -> printf "%d,%d; " x y) ps__ in
        let () = printf "\n" in
        *)
        ps__
    in
    
    let rec dup ps prev n =
        (*let () = printf "step %d, pos cnt %d\n" n (List.length ps) in*)
        match List.find_index ((=)ps) prev with
            | None -> dup (make_step ps 0) (ps :: prev) (n+1)
            | Some i -> (i+1, List.length prev - i-1)
    in
    let (period, start) = dup [p0] [] 0
    in (period, start)


let the_map = parse_map @@ read_lines @@ input_file
let () = printf "size: %d,%d\n" (snd3 the_map) (trd3 the_map)
let p0 = find_start the_map
let () = printf "start: %d,%d\n" (fst p0) (snd p0)
(*
let n = walk the_map p0 64
let () = printf "%d\n" n
*)
let (p,s) = find_cycle the_map p0 64
let () = printf "period %d, start %d\n" p s

let n_ = 26501365
let n = 202300
(*
let n_hor = (the_n - (fst p0) - 1) / w
let n_hor_ = (the_n - (fst p0) - 1) mod w
let n_diag = (the_n - (fst p0) - 1) / w
let n
*)
(*
(26501365 - 64) / 131 = 202300 full blocks to all directions, plus 1 step left
rhomb 404601 x 404601 - (404601^2 + 1)/2 = 81850984601 full blocks, w/o 4 courners
// 202300 * 4 partial blocks which can be paired to form 404600 full blocks
//total 81851389201 full blocks, 40925694601 like initial and 40925694600 in counterphase
total 81850984601 full blocks, 40925492301 like initial and 40925492300 in counterphase
4 blocks with 1 point,

initial block at target step - 7584
counterphase blocks at target step - 7613
overall 40925492301 * 7584 + 40925492300 * 7613 + 4
621947780843788 <
621944706490688 >
621944706494450 >
621944802781893 !=
621944727930768
*)

(* amount of full blocks in phase with initial one *)
(*let n_f_1 = sqr @@ ((n+1)/2)*2-1*)
(*let n_f_2 = sqr @@ (n/2)*2*)
let n_f_1 = sqr (n-1)
let n_f_2 = sqr n

let m_f_1 = walk the_map p0 129
let m_f_2 = walk the_map p0 130

(*corner blocks - start from middle of edges, n*131+66 steps to reach -  walk 130 steps*)
let m_c_u = walk the_map (65,130) 130
let m_c_d = walk the_map (65,0) 130
let m_c_l = walk the_map (0,65) 130
let m_c_r = walk the_map (130,65) 130

(*1/8 blocks - start from edge of edges, n*131+66+66 steps to reach - walk 64 steps*)
let n_18 = n
let m_18_ul = walk the_map (130,130) 64 
let m_18_ur = walk the_map (0,130) 64 
let m_18_dl = walk the_map (130,0) 64 
let m_18_dr = walk the_map (0,0) 64 

(*7/8 blocks - start from edge of edges, (n-1)*131+66+66 steps to reach walk 131+64 steps*)
let n_78 = n-1
let m_78_ul = walk the_map (130,130) 195
let m_78_ur = walk the_map (0,130) 195
let m_78_dl = walk the_map (130,0) 195
let m_78_dr = walk the_map (0,0) 195

let res = n_f_1*m_f_1 + n_f_2*m_f_2 +
    (m_c_u + m_c_d + m_c_l + m_c_r) +
    n_18 * (m_18_ul + m_18_ur + m_18_dl + m_18_dr) +
    n_78 * (m_78_ul + m_78_ur + m_78_dl + m_78_dr)

let () = printf "%d\n" res
