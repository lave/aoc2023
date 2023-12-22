(* generic functions *)

let nvl l r = if Option.is_some l then l else r

let on g f x y = f (g x) (g y)

let on_fst f (l, r) = (f l, r)
let on_snd f (l, r) = (l, f r)

let fst3 (l, _, _) = l
let snd3 (_, m, _) = m
let trd3 (_, _, r) = r

let rcompare x y = compare y x

(* control functions *)

let (<<) f g x = f (g x)

let rec repeat f acc = function
    | 1 -> acc
    | n -> repeat f (f acc) (n - 1)

(* math functions *)

let sqr x = x * x

let rec power a = function
    | 0 -> 1
    | 1 -> a
    | n -> 
        let b = power a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a) 

let rec gcd a b =
    if b <> 0
        then gcd b (a mod b)
        else abs a

let lcm a b =
    match a, b with
        | 0, _ | _, 0 -> 0
        | a, b -> abs (a * b) / (gcd a b)


(* matrix functions *)

let transpose m =
    let h = Array.length m in
    let w = Array.length m.(0) in
    Array.init w (fun y -> Array.init h (fun x -> m.(x).(y)))

let anti_transpose m =
    let h = Array.length m in
    let w = Array.length m.(0) in
    Array.init w (fun y -> Array.init h (fun x -> m.(h - x - 1).(w - y - 1)))

let mirror_h m =
    let h = Array.length m in
    let w = Array.length m.(0) in
    Array.init h (fun y -> Array.init w (fun x -> m.(y).(w - x - 1)))

let mirror_v m =
    let h = Array.length m in
    let w = Array.length m.(0) in
    Array.init h (fun y -> Array.init w (fun x -> m.(h - y - 1).(x)))


(* array functions *)

let sum_a = Array.fold_left (+) 0
let product_a = Array.fold_left ( * ) 1


(* list functions *)

let fold_left1 f l = List.fold_left f (List.hd l) (List.tl l)

let sum = List.fold_left (+) 0
let product = List.fold_left ( * ) 1

let all = List.fold_left (&&) true
let any = List.fold_left (||) false

let minimum (x :: xs) = List.fold_left min x xs
let maximum (x :: xs) = List.fold_left max x xs

let take n l =
    let rec take_ n l acc =
        if n = 0 || List.is_empty l
            then List.rev acc
            else take_ (n - 1) (List.tl l) (List.hd l :: acc)
    in
    take_ n l []

let rec drop n l =
    if n == 0 || l == [] then l
    else drop (n - 1) (List.tl l)


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

(* there's List.delete_assoc but no List.put_assoc *)
let put_assoc label l value =
    let rec put ls =
        if ls = []
            then [(label, value)]
        else
            let l :: ls_ = ls in
            if fst l = label then
                (label, value) :: ls_
            else
                l :: put ls_
    in
    put l

let remove n ns =
    List.filter ((!=) n) ns

(* string functions *)

let to_chars s = List.of_seq @@ String.to_seq s

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let reverse_string s =
    let len = String.length s in
    String.init len (fun i -> s.[len - 1 - i])

let non_empty_strings = List.filter ((<>) 0 << String.length)

let drop_char c s =
    let b = Buffer.create (String.length s) in
    let () = String.iter (fun c_ -> if c_ <> c then Buffer.add_char b c_) s in
    Buffer.contents b

let replace c1 c2 s = String.map (fun c -> if c = c1 then c2 else c) s


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


(* print funtcions *)

let print_ints title ns =
    let () = if title <> "" then Printf.printf "%s: " title else () in
    let () = List.iter (Printf.printf "%d, ") ns in
    Printf.printf "\n"

let print_chars title cs =
    let () = if title <> "" then Printf.printf "%s: " title else () in
    let () = List.iter (Printf.printf "%c, ") cs in
    Printf.printf "\n"

let print_strs title ss =
    let () = if title <> "" then Printf.printf "%s: " title else () in
    let () = List.iter (Printf.printf "%s, ") ss in
    Printf.printf "\n"

(* memoization *)

let memoize f =
    let memo = Hashtbl.create 1000 in

    fun x ->
        let memoized = Hashtbl.find_opt memo x in
        if Option.is_some memoized then
            Option.get memoized
        else
            let r = f x in
            let () = Hashtbl.add memo x r in
            r

