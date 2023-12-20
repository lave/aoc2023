open Common
open Printf

type module_type =
    | FlipFlop
    | Conjunction
    | Broadcast
    | Button
    | Nop

let type_to_string = function
    | FlipFlop -> "FF"
    | Conjunction -> "Conj"
    | Broadcast -> "BR"
    | _ -> ""

type module_state =
    | FlipFlopS of bool
    | ConjunctionS of (string * bool) list
    | EmptyS

let parse_module s =
    let [name; outs] = String.split_on_char '-' s in
    let name = String.trim name in
    let type_ = String.get name 0 in
    let (type__, name__) = match type_ with
        | '%' -> (FlipFlop, String.sub name 1 @@ String.length name - 1)
        | '&' -> (Conjunction, String.sub name 1 @@ String.length name - 1)
        | _ -> let t = if name = "broadcaster" then Broadcast else Nop in (t, name)
    in
    let outputs = List.map String.trim @@ String.split_on_char ',' @@ String.sub outs 1 @@ String.length outs - 1 in
    (name__, type__, outputs)

let make_states modules =
    let inputs name =
        List.map fst3 @@ List.filter (fun m -> List.mem name (trd3 m)) modules
    in
    let make_state m =
        match m with
            | (_, FlipFlop, _) -> FlipFlopS false
            | (name, Conjunction, _) ->
                    let ins = List.map (fun n -> (n, false)) @@ inputs name in
                    ConjunctionS ins
            | _ -> EmptyS
    in
    List.map make_state modules

let emulate n modules states =
    let consume (from, to_, value) =
        (*let () = printf "%s --%s--> %s\n%!" from (if value then "high" else "low") to_ in*)
        let module_ = Hashtbl.find_opt modules to_ in
        if module_ = None then
            []
        else
            let Some (_, type_, outs) = module_ in
            (*let () = printf "%s --%s--> %s %s\n%!" from (if value then "high" else "low") to_ (type_to_string type_) in*)
            match type_ with
                | FlipFlop -> 
                    if value then
                        []
                    else
                        let FlipFlopS v = Hashtbl.find states to_ in
                        let v_ = not v in
                        let () = Hashtbl.replace states to_ @@ FlipFlopS v_ in
                        List.map (fun n -> (to_, n, v_)) outs
                | Conjunction ->
                    let ConjunctionS ins = Hashtbl.find states to_ in
                    let ins_ = put_assoc from ins value in
                    let () = Hashtbl.replace states to_ @@ ConjunctionS ins_ in
                    let value_ = List.find_opt (fun (_, v) -> not v) ins_ != None in
                    List.map (fun n -> (to_, n, value_)) outs
                | Broadcast ->
                    List.map (fun n -> (to_, n, value)) outs
                | _ -> []
    in

    let rec emulate_ ss pulses nl nh rx=
        if pulses = [] then (nl, nh, rx)
        else
            let pulse :: pulses_ = pulses in
            let new_pulses = consume pulse in
            (* xc -1-> zh - 3846
               th -1-> zh - 4000
               pd -1-> zh - 3876
               bp -1-> zh - 3822
               *)
            let found = (List.find_opt (fun (from, to_, v) -> from = "bp" && to_ = "zh" && v) new_pulses) != None in
            let pulses__ = pulses_ @ new_pulses in
            emulate_ ss pulses__ (if trd3 pulse then nl else nl + 1) (if trd3 pulse then nh + 1 else nh) (if found then rx+1 else rx)
    in

    let rec emulate_rx i =
        let () = if i mod 100000 = 0 then printf "press %d\n%!" i else () in
        let (_, _, rx) = emulate_ states [("button", "broadcaster", false)] 0 0 0 in
        let () = if rx = 1 then printf "Found %d\n%!" i else () in
        emulate_rx (i+1)
    in


    (*
    let res = List.init n @@ fun i ->
        let () = printf "press %d\n" i in
        emulate_ states [("button", "broadcaster", false)] 0 0 0 in
    (sum @@ List.map fst res, sum @@ List.map snd res)
    *)
    emulate_rx 0

(*let button = (Button, "broadcast") in*)
let modules = List.map parse_module @@ read_lines @@ input_file
let states = make_states modules

let modules_ = Hashtbl.of_seq @@ List.to_seq @@ List.map (fun m -> (fst3 m, m)) modules
let () = printf "%d modules\n" @@ List.length modules
let states_ = Hashtbl.of_seq @@ List.to_seq @@ List.map2 (fun m s -> (fst3 m, s)) modules states
let () = printf "%d states\n" @@ List.length states

(*
let (nl, nh) = emulate 1000 modules_ states_
let () = printf "%d\n" (nl * nh)
*)
(*
let n = emulate 1000 modules_ states_
let () = printf "%d\n" n
*)
let n = List.fold_left lcm 3847 [4001; 3877; 3823]
let () = printf "%d\n" n
