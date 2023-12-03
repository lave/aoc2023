let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []


let sum = List.fold_left (+) 0
let product = List.fold_left ( * ) 1


let explode_string s : char list = List.init (String.length s) (String.get s)

let reverse_string s =
    let len = String.length s in
    String.init len (fun i -> s.[len - 1 - i])


let nvl l r = if Option.is_some l then l else r

let (<<) f g x = f (g x)
let on_fst f (l, r) = (f l, r)
let on_snd f (l, r) = (l, f r)

let fst3 (l, _, _) = l
let snd3 (_, m, _) = m
let trd3 (_, _, r) = r
