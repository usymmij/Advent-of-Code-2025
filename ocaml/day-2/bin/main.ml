open Printf

(* read the entire file *)
let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let filename = "input.txt";;


let get_first = function 
    | [] -> ""
    | a :: rest -> a
;;
let ranges = read_file filename |> String.split_on_char '\n'|> get_first |> String.split_on_char ','
;;

let to_tuple elem = 
    let tuplize_str = function
        | [] -> ("","")
        | a :: b ::rest -> (a,b)
    in

    String.split_on_char '-' elem |> tuplize_str
;;

let ranges = List.map to_tuple ranges;;

(* 

let rec print_tuple_list = function
    | [] -> ()
    | st1::r -> match st1 with
        | (a,b) -> 
            printf "%s %s \n" a b;
            print_tuple_list r
;;

let _ = print_tuple_list ranges;;
let _ = print_endline "";;
let _ = print_endline "";;
let _ = print_endline "";;

*)

let rec pow10 x = match x with
    | -1 -> 0
    | 0 -> 1
    | n -> 10 * (pow10 (n-1))
;;


let filter_divisible nmod string = 
    let rec string_9s l = match l with
        | 0 -> ""
        | n -> string_9s (n-1) ^ "9"
    in
    let len_divisible = (mod) (String.length string) nmod
    in
    match len_divisible with
        | 0 -> string
        | n -> string_9s ((String.length string) - n)
;;

let rec len_inv_ids halflength = match halflength with
    | 0 -> 0
    | 1 -> 9
    | n -> ((pow10 @@ (n - 1)) * 9) + len_inv_ids (n-1) ;;

let count_inv_ids numstr = 
    let halflen = (String.length numstr) / 2 in
    let p1 = String.sub numstr 0 halflen in
    let p2 = String.sub numstr halflen halflen in
    let int_of_string_zero_empty st = match st with
        | "" -> 0
        | x -> int_of_string x
    in
    let eq_check = ((int_of_string_zero_empty p1) - (int_of_string_zero_empty p2) <= 0) && (String.length p1 != 0) in
    let inv_ids x = x - (pow10 (halflen-1)) in
    match numstr with
    | "" -> 0
    | a -> 
        let base = (len_inv_ids @@ halflen - 1) in 
        let n_dig = (inv_ids @@ int_of_string_zero_empty p1) in
        let extra = (if eq_check then 1 else 0) in
        base + n_dig + extra
;;

let rec summed_inv_ids_from_count base count = match count with
        | 0 -> 0
        | n -> let st = string_of_int n in
            let rec repeat base string = match base with
                | 1 -> string
                | k -> string ^ (repeat (k-1) string)
            in
            (int_of_string (repeat base st)) + (summed_inv_ids_from_count base (n-1))
;;

let rec sum_inv_ids = function
    | [] -> 0
    | st1::r -> match st1 with
        | (a,b) -> 
            let ids_a = count_inv_ids @@ filter_divisible 2 @@ string_of_int @@ (int_of_string a) - 1 in
            let ids_b = count_inv_ids @@ filter_divisible 2 b in
            ((summed_inv_ids_from_count 2 @@ ids_b) - (summed_inv_ids_from_count 2 @@ ids_a)) + (sum_inv_ids r)
;;

let _ = printf "part 1: %d\n" @@ sum_inv_ids ranges;;

(* part 2 *)

(* the algorithm from p1 could be adapted to p2, and would be more wayyy more efficient *)
(* but I have work to do so I'm doing the naive way instead *)

let non_1_factors n = 
    let rec _factors n m = match m with
        | 1 -> []
        | k -> List.append (_factors n (m-1)) @@ if (n mod m == 0) then [m] else []
    in 
    _factors n n
;;

let is_invalid numstr = 
    let length = String.length numstr in
    let len_factors = non_1_factors length in
    let check_factor fac = 
        let portion_len = length / fac in
        let first_p = String.sub numstr 0 portion_len in
        let rec check_portion n = 
                if (n == fac) then true else (
                let x = String.sub numstr (portion_len * n) portion_len in
                ((x = first_p) && (check_portion (n+1)))
                )
        in
        check_portion 1
    in  
    let rec check_factors facs = match facs with
        | [] -> false
        | factor :: rest -> (check_factor factor) || (check_factors rest)
    in
    check_factors len_factors
;;

let rec check_invalids x y = 
    let added = if (is_invalid x) then (int_of_string x) else 0 in
    if (x = y) then added else (added + 
    (check_invalids (string_of_int @@ (int_of_string x)+1) y)
    )
;;

let rec sum_inv_ids_p2 = function
    | [] -> 0
    | st1::r -> match st1 with
        | (a,b) -> 
            (check_invalids a b) + (sum_inv_ids_p2 r)
;;

let _ = printf "part 2: %d" @@ sum_inv_ids_p2 ranges;;
