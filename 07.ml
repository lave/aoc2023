open Common
open Printf

type combination =
    | HighCard
    | OnePair
    | TwoPairs
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

let card_rank = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | '*' -> 0
    | c -> int_of_char c

let find_type cards =
    let unique_cards = List.sort_uniq compare cards in
    let counts = List.sort rcompare @@ List.map (fun c -> List.length @@ List.filter ((=) c) cards) unique_cards in

    let type_ = match counts with
        | 5 :: _      -> FiveOfAKind
        | 4 :: _      -> FourOfAKind
        | 3 :: 2 :: _ -> FullHouse
        | 3 :: _      -> ThreeOfAKind
        | 2 :: 2 :: _ -> TwoPairs
        | 2 :: _      -> OnePair
        | 1 :: _      -> HighCard
    in
    (*let () = printf "type %d\n" type_ in*)
    type_

let find_type_with_jokers cards =
    let (jokers, others) = List.partition ((=) 0) cards in
    let jokers_count = List.length jokers in

    let type_ = if jokers_count >= 4
        (* with 5 or 4 jokers we can make five of a kind *)
        then FiveOfAKind
        else match (jokers_count, find_type others) with
            | (3, OnePair) -> FiveOfAKind
            | (3, HighCard) -> FourOfAKind
            | (2, ThreeOfAKind) -> FiveOfAKind
            | (2, OnePair) -> FourOfAKind
            | (2, HighCard) -> ThreeOfAKind
            | (1, FourOfAKind) -> FiveOfAKind
            | (1, ThreeOfAKind) -> FourOfAKind
            | (1, TwoPairs) -> FullHouse
            | (1, OnePair) -> ThreeOfAKind
            | (1, HighCard) -> OnePair
            | (0, t) -> t
    in
    (*let () = printf "type %d\n" type_ in*)
    type_


let parse_hand line = 
    let [cards; bid] = String.split_on_char ' ' line in
    (cards, int_of_string bid)

let rank (cards, bid) =
    let cards_ = List.map card_rank @@ to_chars cards in
    let type_ = find_type_with_jokers cards_ in
    let weight = List.fold_left (fun acc c -> acc * 15 + c) 0 cards_ in
    (cards, bid, (type_, weight))

let score hands =
    fst @@ List.fold_left (fun (sum, i) hand -> (sum + i * (snd3 hand), i + 1)) (0, 1) hands


let hands = List.map parse_hand @@ read_lines @@ input_file

let ranked = List.map rank hands
let sorted = List.sort (on trd3 compare) ranked
let () = printf "%d\n" @@ score sorted

(* replace J with * *)
let hands_with_jokers = List.map (on_fst @@ replace 'J' '*') hands
let ranked = List.map rank hands_with_jokers
let sorted = List.sort (on trd3 compare) ranked
let () = printf "%d\n" @@ score sorted
