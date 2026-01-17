open Printf

(* read the entire file *)
let read_file file =
  In_channel.with_open_bin file In_channel.input_all

(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents


let get_dir = function
    | 'L' -> -1
    | 'R' -> 1
;;

let process_line = function 
    | "" -> None
    | line -> 
    let numstr = String.sub line 1 ((String.length line) - 1) in
    Some ((get_dir (String.get line 0)) * (int_of_string numstr))
    ;;

let rec process_lines = function
    | [] -> [] (* this option only runs if the arg is originally empty, since process_line will always return None for the last value *)
    | line ::  rest_of_lines -> let maybe_val = process_line line in
        match maybe_val with
        | Some v -> List.append [v] (process_lines rest_of_lines)
        | None -> []

let filename = "input.txt";;
let rotations = (process_lines (read_lines filename));;

let modulus value equiv_class = 
    let res = (value mod equiv_class) in
    if res < 0 then res + equiv_class else res
;;

let rec count_zeros_part_1 dial rotlist = 
    match rotlist with
    | [] -> 0
    | rot::rest -> let new_dial = modulus (rot + dial) 100 in
        (if new_dial == 0 then 1 else 0) + (count_zeros_part_1 new_dial rest)
;;

(*List.iter (printf "%d\n") rotations;;*)
let zeros = count_zeros_part_1 50 rotations;;
printf "part 1: %d\n" zeros;;

let abs x = if x < 0 then (~-1) * x else x;;

let rec count_zeros_part_2 dial rotlist = 
    match rotlist with
    | [] -> 0
    | rot::rest -> let new_dial = modulus (rot + dial) 100 in
        (
            let dist_to_zero = if rot >= 0 then 100 - dial else (modulus (dial-1) 100) + 1 in
            abs (rot / 100) + (if abs (rot mod 100) >= dist_to_zero then 1 else 0) + (count_zeros_part_2 new_dial rest)
        )
;;

let zeros = count_zeros_part_2 50 rotations;;
printf "part 1: %d\n" zeros;

