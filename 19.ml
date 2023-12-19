open Common
open Printf

let parse_rule s =
    let tokens = String.split_on_char ':' s in
    if List.length tokens = 1 then
        let [action] = tokens in
        (None, action)
    else
        let [cond; action] = tokens in
        let op = if String.contains cond '<' then '<' else '>' in
        let [category; number] = String.split_on_char op cond in
        (*let () = printf "cat=%s, num=%s\n" category number in*)
        (Option.some (category, op, int_of_string number), action)

let parse_workflow s =
    let [name; def] = String.split_on_char '{' s in
    let def_ = String.sub def 0 (String.length def - 1) in
    let rules = List.map parse_rule @@ String.split_on_char ',' def_ in
    (name, rules)

let parse_category s =
    let [name; number] = String.split_on_char '=' s in
    (*let () = printf "name=%s, num=%s\n" name number in*)
    (name, int_of_string number)

let parse_part s =
    let def = String.sub s 1 (String.length s - 2) in
    let categories = List.map parse_category @@ String.split_on_char ',' def in
    (List.assoc "x" categories, List.assoc "m" categories, List.assoc "a" categories, List.assoc "s" categories)


let is_accepted workflows part =
    let (x,m,a,s) = part in
    let rec is_acc name =

        let rec is_acc_ rules =
            let r :: rs = rules in
            let (cond, action) = r in
            let pass = if cond = None then true
            else
                let (cat, op, n) = Option.get cond in
                let n_ = match cat with
                    | "x" -> x
                    | "m" -> m
                    | "a" -> a
                    | "s" -> s
                in
                match op with
                    | '<' -> n_ < n
                    | '>' -> n_ > n
            in

            if pass then
                match action with
                    | "A" -> true
                    | "R" -> false
                    | wf -> is_acc wf
            else
                is_acc_ rs
        in

        let rules = Hashtbl.find workflows name in
        is_acc_ rules
    in
    is_acc "in"

let combs ((x1,x2),(m1,m2),(a1,a2),(s1,s2)) =
    (x2-x1+1)*(m2-m1+1)*(a2-a1+1)*(s2-s1+1)

let n_accepted workflows =
    let rec n_acc name ds =
        let rec n_acc_ rules ds_ =
            let (dx,dm,da,ds)=ds_ in
            let ((x1,x2),(m1,m2),(a1,a2),(s1,s2))=ds_ in
            if x2<x1 || m2<m1 || a2<a1 || s2<s1 then 0
            else
                let r :: rs = rules in
                let (cond, action) = r in
                let (passed, not_passed) = match cond with
                    | None -> (ds_, ((2,1),dm,da,ds))
                    | Some ("x", '<', n) -> (((x1,n-1),dm,da,ds), ((n,x2),dm,da,ds))
                    | Some ("x", '>', n) -> (((n+1,x2),dm,da,ds), ((x1,n),dm,da,ds))
                    | Some ("m", '<', n) -> ((dx,(m1,n-1),da,ds), (dx,(n,m2),da,ds))
                    | Some ("m", '>', n) -> ((dx,(n+1,m2),da,ds), (dx,(m1,n),da,ds))
                    | Some ("a", '<', n) -> ((dx,dm,(a1,n-1),ds), (dx,dm,(n,a2),ds))
                    | Some ("a", '>', n) -> ((dx,dm,(n+1,a2),ds), (dx,dm,(a1,n),ds))
                    | Some ("s", '<', n) -> ((dx,dm,da,(s1,n-1)), (dx,dm,da,(n,s2)))
                    | Some ("s", '>', n) -> ((dx,dm,da,(n+1,s2)), (dx,dm,da,(s1,n)))
                in
                let n_passed = match action with
                    | "A" -> combs passed
                    | "R" -> 0
                    | wf -> n_acc wf passed
                in
                let n_not_passed = n_acc_ rs not_passed in
                n_passed + n_not_passed
        in

        let rules = Hashtbl.find workflows name in
        n_acc_ rules ds
    in
    n_acc "in" ((1,4000),(1,4000),(1,4000),(1,4000))


let [workflows_; parts_] = split_by (fun s -> String.length s = 0) @@ read_lines @@ input_file
let workflows = Hashtbl.of_seq @@ List.to_seq @@ List.map parse_workflow workflows_
let parts = List.map parse_part parts_

let accepted = List.filter (is_accepted workflows) parts
let n = sum @@ List.map (fun (x ,m, a, s) -> x+m+a+s) accepted
let () = printf "%d\n" n

let n1 = n_accepted workflows
let () = printf "%d\n" n1
