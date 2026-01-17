open Printf

(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let process_line = function 
    | "" -> None
    | line -> 
        let sub48 x = x - 48 in
        Some ((List.map sub48) @@ (List.map (int_of_char)) @@ List.init (String.length line) (String.get line))
;; 

let rec process_lines = function
    | [] -> [] (* this option only runs if the arg is originally empty, since process_line will always return None for the last value *)
    | line ::  rest_of_lines -> let maybe_val = process_line line in
        match maybe_val with
        | Some v -> List.append [v] (process_lines rest_of_lines)
        | None -> []

let filename = "test.txt";;
let int_arrs = (process_lines (read_lines filename));;

let shift_in_2d digit arr = 
    match arr with
    | [] -> [digit;]
    | [a] -> [digit; a]
    | [a; b] -> if digit < a then [a; b]
        else 
        (
            if a < b then [digit; b]
            else [digit; a]
        )
;;

let rec find_biggest_2d_number = function
    | [] -> []
    | digit :: rest -> shift_in_2d digit (find_biggest_2d_number rest)
;;

let list_of_list_of_ans_2d = List.map find_biggest_2d_number int_arrs;;

let int_of_list list =
    let rec int_of_list_ sum list = 
        match list with
        | [] -> sum
        | a :: rest -> int_of_list_ (sum * 10 + a) rest
        in
    int_of_list_ 0 list
;;

let rec sum =function
    | [] -> 0
    | a :: r -> a + sum r
;;

printf "part 1: %d\n" @@ sum @@ List.map int_of_list list_of_list_of_ans_2d;;

let shift_in_nd n digit arr = 
    let templist = (List.append [digit] arr) in
    if List.length templist <= n  (* if the candidate isn't full *)
    then templist
    else 
    let rec scan_2_chars = function
        | [] -> []
        | a :: b :: rest -> if a < b then List.append [b] rest
                else List.append [a] @@ scan_2_chars @@ List.append [b] rest
        | [_] -> []
    in
    scan_2_chars templist
;;

let rec find_biggest_nd_number n list = 
    match list with 
    | [] -> []
    | digit :: rest -> shift_in_nd n digit (find_biggest_nd_number n rest)
;;

(* int_arrs is a 2d array of (single digit) ints, each row is one line *)
let list_of_list_of_ans_nd = List.map (find_biggest_nd_number 12) int_arrs in
printf "part 2: %d\n" @@ sum @@ List.map int_of_list list_of_list_of_ans_nd;;

