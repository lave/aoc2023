(* generic functions *)

let nvl l r = if Option.is_some l then l else r

let on g f x y = f (g x) (g y)

let on_fst f (l, r) = (f l, r)
let on_snd f (l, r) = (l, f r)

let fst3 (l, _, _) = l
let snd3 (_, m, _) = m
let trd3 (_, _, r) = r


(* control functions *)

let (<<) f g x = f (g x)


(* math functions *)

let rec power a = function
    | 0 -> 1
    | 1 -> a
    | n -> 
        let b = power a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a) 


(* list functions *)

let sum = List.fold_left (+) 0
let product = List.fold_left ( * ) 1

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

let minimum (x :: xs) = List.fold_left min x xs
let maximum (x :: xs) = List.fold_left max x xs

let split_by is_separator =
    let rec split dst src =
        if List.is_empty src
            then List.rev @@ List.map List.rev dst
            else
                let x :: xs = src in
                if is_separator x
                    then split ([] :: dst) xs
                    else split ((x :: List.hd dst) :: List.tl dst) xs
    in
    split [[]]

let split_by_count n =
    let rec split c dst src =
        if List.is_empty src
            then List.rev @@ List.map List.rev dst
            else
                let x :: xs = src in
                if c = n
                    then split 1 ([x] :: dst) xs
                    else split (c + 1) ((x :: List.hd dst) :: List.tl dst) xs
    in
    split 0 [[]]


(* string functions *)

let explode_string s : char list = List.init (String.length s) (String.get s)

let reverse_string s =
    let len = String.length s in
    String.init len (fun i -> s.[len - 1 - i])

let non_empty_strings = List.filter ((<>) 0 << String.length)

let drop_char c s =
    let b = Buffer.create (String.length s) in
    let () = String.iter (fun c_ -> if c_ <> c then Buffer.add_char b c_) s in
    Buffer.contents b


(* input functions *)

let input_file = if Array.length Sys.argv > 1 then Sys.argv.(1) else Sys.argv.(0) ^ ".input"

let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []
