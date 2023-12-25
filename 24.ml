open Common
open Printf

let parse_hail_f line =
    let [pos; speed] = String.split_on_char '@' line in
    let [x; y; z] = List.map (float_of_string << String.trim) @@ String.split_on_char ',' pos in
    let [dx; dy; dz] = List.map (float_of_string << String.trim) @@ String.split_on_char ',' speed in
    ((x, y, z), (dx, dy, dz))

let parse_hail line =
    let [pos; speed] = String.split_on_char '@' line in
    let [x; y; z] = List.map (int_of_string << String.trim) @@ String.split_on_char ',' pos in
    let [dx; dy; dz] = List.map (int_of_string << String.trim) @@ String.split_on_char ',' speed in
    ((x, y, z), (dx, dy, dz))

let divf x y =
    (float_of_int x) /. (float_of_int y)

let find_intersections hails =
    (*
    let xi = 200000000000000 in
    let xa = 400000000000000 in
    let yi = 200000000000000 in
    let ya = 400000000000000 in
*)
    let xi = 200000000000000. in
    let xa = 400000000000000. in
    let yi = 200000000000000. in
    let ya = 400000000000000. in
    (*
    let xi = 7 in
    let xa = 27 in
    let yi = 7 in
    let ya = 27 in
*)


    let intersects i1 ((x1, y1, _), (dx1, dy1, _)) i2 ((x2, y2, _), (dx2, dy2, _)) =
        let title = sprintf "%d x %d" (i1+1) (i2+1) in
        if (dx1 = 0. && dx2 = 0.) || (dx1 *. dy2 = dx2 *. dy1) then
            (* parallel *)
            let () = printf "%s: false - parallel\n" title in
            false
        else
            let (a1, b1, c1) = (dx1, dy1, dx1*.y1 -. dy1*.x1) in
            let (a2, b2, c2) = (dx2, dy2, dx2*.y2 -. dy2*.x2) in

            let x = (c2*.a1 -. c1*.a2) /. (b1*.a2 -. b2*.a1) in
            let y = (c2*.b1 -. c1*.b2) /. (b1*.a2 -. b2*.a1) in
            if x < xi || x > xa || y < yi || y > ya then
                let () = printf "%s: false - intersect at %f, %f, outside\n" title x y in
                false
            else
                let future1 = if dx1 = 0. then
                    (y -. y1) *. dy1 >= 0.
                else
                    (x -. x1) *. dx1 >= 0.
                in
                let future2 = if dx2 = 0. then
                    (y -. y2) *. dy2 >= 0.
                else
                    (x -. x2) *. dx2 >= 0.
                in
                (*
                let future1_ = if dy1 = 0. then
                    (x -. x1) *. dx1 >= 0.
                else
                    (y -. y1) *. dy1 >= 0.
                in
                let future2_ = if dy2 = 0. then
                    (x -. x2) *. dx2 >= 0.
                else
                    (y -. y2) *. dy2 >= 0.
                in
                let () = if future2 != future2_ then
                    printf "!!!! x=%f y=%f x2=%f y2=%f dx2=%f dy2=%f check x=%f check y=%f " x y x2 y2 dx2 dy2 ((x -. x2) *. dx2) ((y -. y2) *. dy2)
                else
                    ()
                in
                *)
                let res = future1 && future2 in
                let () = printf "%s: %b - intersect at %f, %f, f1=%b, f2=%b\n" title res x y future1 future2 in
                res
    in

    let rec find hs n i1 =
        if hs = [] then
            n
        else
            let h :: hs_ = hs in
            let n_ = List.length @@ List.filteri (intersects i1 h) hs_ in
            find hs_ (n+n_) (i1+1)
    in

    find hails 0 0


let print_equasions hails = 
    let print (a,b,c,d,e,f) =
        printf "%d*x+%d*y+%d*z+%d*x*y+%d*x*z+%d*y*z=0\n" a b c d e f
    in

    let h1 :: h2 :: h3 :: _ = hails in
    let ((x1,y1,z1),(dx1,dy1,dz1)) = h1 in
    let ((x2,y2,z2),(dx2,dy2,dz2)) = h2 in
    let ((x3,y3,z3),(dx3,dy3,dz3)) = h3 in
    let coeff_x = (x3-x2, x1-x3, x2-x1, dx1-dx2, dx3-dx1, dx2-dx3) in
    let coeff_y = (y3-y2, y1-y3, y2-y1, dy1-dy2, dy3-dy1, dy2-dy3) in
    let coeff_z = (z3-z2, z1-z3, z2-z1, dz1-dz2, dz3-dz1, dz2-dz3) in
    let () = print coeff_x in
    let () = print coeff_y in
    let () = print coeff_z in
    ()

let solve hails t1 t2 =
    let h1 :: h2 :: h3 :: _ = hails in
    let ((x1,y1,z1),(dx1,dy1,dz1)) = h1 in
    let ((x2,y2,z2),(dx2,dy2,dz2)) = h2 in
    let dx = (x2 + dx2*t2 - x1 - dx1*t1) / (t2 - t1) in
    let dy = (y2 + dy2*t2 - y1 - dy1*t1) / (t2 - t1) in
    let dz = (z2 + dz2*t2 - z1 - dz1*t1) / (t2 - t1) in
    let x = x1 + dx1*t1 - dx*t1 in
    let y = y1 + dy1*t1 - dy*t1 in
    let z = z1 + dz1*t1 - dz*t1 in
    ((x,y,z),(dx,dy,dz))

(*
let hails = List.map parse_hail_f @@ read_lines @@ input_file

let n = find_intersections hails
let () = printf "%d\n" n
*)

let hails = List.map parse_hail @@ read_lines @@ input_file

let () = print_equasions hails
(*
t1 = 666003776903
t2 = 654152070134
t3 = 779453185471
*)
let ((x,y,z),_) = solve hails 666003776903 654152070134
let () = printf "%d\n" (x+y+z)
