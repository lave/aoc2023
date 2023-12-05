open Common
open Printf


let parse_seeds line = 
    let ["seeds"; seeds_str] = String.split_on_char ':' line in
    List.map int_of_string @@ String.split_on_char ' ' @@ String.trim seeds_str

let parse_mapping lines = 
    let parse_mapping_range line =
        let [d; s; n] = List.map int_of_string @@ String.split_on_char ' ' line in
        (* mapping range - (destination, source, amount) *)
        (d, s, n)
    in
    (* ignore first line since it's static header like 'x-to-y map' *)
    List.map parse_mapping_range @@ List.tl lines

let apply_mapping src mapping =
    let apply_mapping_to x =
        let mapping_range = List.find_opt (fun (d, s, n) -> x >= s && x < s + n) mapping in
        let mapped = Option.map (fun (d, s, _) -> x - s + d) mapping_range in
        Option.value mapped ~default:x
    in
    List.map apply_mapping_to src


let lines = read_lines @@ input_file
let [seeds_line] :: mappings_lines = split_by ((=) String.empty) lines

let seeds = parse_seeds @@ seeds_line
let maps = List.map parse_mapping mappings_lines


let locations = List.fold_left apply_mapping seeds maps
let lowest = minimum locations
let () = printf "%d\n" lowest


(*
let print_ranges title ranges  =
    let () = printf "%s: " title in
    let () = List.iter (fun (b, e) -> printf "[%d, %d), " b e) ranges in
    printf "\n"
*)

let merge_ranges ranges =
    (* sort ranges by beginning *)
    let sorted = List.sort (on fst (-)) ranges in

    let merge_ merged_ranges new_range =
        let last_merged_range = List.hd merged_ranges in
        let (b_, e_) = last_merged_range in
        let (b, e) = new_range in
        (* since ranged are sorted, new range starts after last merged range *)
        if b <= e_
            (* ranges intersect *)
            then
                if e <= e_
                    (* new range is already fully covered by last merged range - just skip it *)
                    then merged_ranges
                    (* new range ends after last merged range - extend last merged range *)
                    else (b_, max e e_) :: (List.tl merged_ranges)
        else
            (* ranges don't intersect - new range is to the right of the last merged range - add new range *)
            new_range :: merged_ranges
    in

    let merged = List.fold_left merge_ [List.hd sorted] @@ List.tl sorted in
    List.rev merged


let apply_range_mapping src mapping =
    let apply_mapping_rule_to_range (d, s, n) acc (b, e) =
        let b_ = s in
        let e_ = s + n in
        let delta = d - s in
        let map (b, e) = (b + delta, e + delta) in

        let (non_mapped, mapped) =
            (* mapping rule doesn't intersect the range - keep the range in non-mapped list *)
            if e <= b_ || b >= e_ then ([(b, e)], [])
            (* mapping rule fully covers the range - move the range to mapped list *)
            else if b >= b_ && e <= e_ then ([], [map (b, e)])
            (* mapping rule covers right part of the range - keep left part in non-mapped list and move right part to mapped list *)
            else if b < b_ && e <= e_ then ([(b, b_)], [map (b_, e)])
            (* mapping rule covers left part of the range - keep right part in non-mapped list and move left part to mapped list *)
            else if b >= b_ && e > e_ then ([(e_, e)], [map (b, e_)])
            (* mapping rule covers middle of the range - keep left and right parts in non-mapped list and move middle part to mapped list *)
            else if b < b_ && e > e_ then ([(b, b_); (e_, e)], [map (b_, e_)])
            (* this branch should never be reachable *)
            else failwith "should never be here" in

        let (old_non_mapped, old_mapped) = acc in
        (non_mapped @ old_non_mapped, mapped @ old_mapped)
    in

    let apply_mapping_rule (non_mapped, mapped) rule =
        List.fold_left (apply_mapping_rule_to_range rule) ([], mapped) non_mapped
    in

    (* let () = print_ranges "Ranges before" src in *)
    let (non_mapped, mapped) = List.fold_left apply_mapping_rule (src, []) mapping in
    let dst = merge_ranges (non_mapped @ mapped) in
    (* let () = print_ranges "Ranges after" dst in *)
    dst


(* now seeds are defined by ranges, not by numbers *)
let seeds_ranges = merge_ranges @@ List.map (fun [b; n] -> (b, b + n)) @@ split_by_count 2 seeds 

let locations = List.fold_left apply_range_mapping seeds_ranges maps
(* ranges are sorted, so lowest value is beginning of the first range *)
let lowest = fst @@ List.hd locations
let () = printf "%d\n" lowest
