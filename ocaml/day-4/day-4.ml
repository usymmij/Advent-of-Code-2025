open Printf

(* read lines *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents
;;

let lines = read_lines "input.txt";;

(* let _ = List.map (printf "%s\n") lines;; *)

let len = match lines with 
    | [] -> 0
    | line::rest -> String.length (line)
;;

let newbuffer () = (Array.make len (Some 0));;
let invalidbuffer () = (Array.make len (None));;
let get_validity = function 
        | (a,b) -> printf "%d, %d\n" a b
;;

let found_char index topbuffer middle_buffer bottom_buffer =
    let incr index buffer =
        let temp = buffer.(index) in
        match temp with
            | None -> ()
            | Some ctr -> buffer.(index) <- Some (ctr + 1)
    in
    let _ = if (index > 0) then
        [
            incr (index-1) topbuffer,
            incr (index-1) middle_buffer,
            incr (index-1) bottom_buffer
        ] else [(), (), ()] in
    let _ = if (index < len - 1) then
        [   incr (index+1) topbuffer,
            incr (index+1) middle_buffer,
            incr (index+1) bottom_buffer
        ] else [(), (), ()] in
    incr (index) topbuffer;
    incr (index) bottom_buffer
;;

let rec count_accessible_bottom_buffer bottom_buffer = 
    let is_accessible coord =
        match coord with 
            | None -> 0
            | Some a -> if a < 4 then 1 else 0
    in
    let sum_accessible sum element = sum + is_accessible element in
    Array.fold_left sum_accessible 0 bottom_buffer
;;

let rec accessible_rolls rows bottom_buffer middle_buffer =
    match rows with
        | row :: rest ->
            let topbuffer = newbuffer () in
            let invalidate index buffer = buffer.(index) <- None in
            let rec tally_rolls index =
                if index = len then ()
                else
                    let char = row.[index] in
                    match char with 
                    | '@' -> found_char index topbuffer middle_buffer bottom_buffer;
                        tally_rolls (index + 1)
                    | x -> invalidate index middle_buffer;
                        tally_rolls (index + 1)
            in
            let _ = if row = "" then () else tally_rolls 0 in
            let accessible = count_accessible_bottom_buffer bottom_buffer in
            accessible + (accessible_rolls rest middle_buffer topbuffer)
        | [] -> 0;;
;;
let sum = accessible_rolls lines (invalidbuffer ()) (newbuffer ());;
let _ = printf "part 1: %d\n" sum;;

type rowstate = {i: int; ct: int; newrow: string}


let rec remove_accessible_bottom_buffer bottom_buffer row= 
    let is_accessible coord =
        match coord with 
            | None -> 0
            | Some a -> if a < 4 then 1 else 0
    in
    let sum_accessible rwstate element = 
        let accs = is_accessible element in
        {i=rwstate.i + 1; 
            ct=rwstate.ct + accs;
            newrow= rwstate.newrow ^ if (accs = 1) || (element = None) then "." else "@"
        }
    in
    Array.fold_left sum_accessible {i=0;ct=0;newrow=""} bottom_buffer
;;

type processed_rows = {sum: int; rows: string list}
let rec remove_accessible_rolls rows bottom_buffer middle_buffer =
    match rows with
        | row :: rest ->
            let topbuffer = newbuffer () in
            let invalidate index buffer = buffer.(index) <- None in
            let rec tally_rolls index =
                if index = len then ()
                else
                    let char = row.[index] in
                    match char with 
                    | '@' -> found_char index topbuffer middle_buffer bottom_buffer;
                        tally_rolls (index + 1)
                    | x -> invalidate index middle_buffer;
                        tally_rolls (index + 1)
            in
            let _ = if row = "" then () else tally_rolls 0 in
            let state = remove_accessible_bottom_buffer bottom_buffer row in
            let prows = remove_accessible_rolls rest middle_buffer topbuffer in
            {sum = prows.sum + state.ct; rows = [state.newrow] @ prows.rows}
        | [] -> {sum=0; rows=[]};;
;;

let rec remove_possible rows =
    let removed_ = remove_accessible_rolls rows (invalidbuffer ()) (newbuffer ()) in
    let removed = {
        sum = removed_.sum;
        rows = match removed_.rows with
            | a::b -> b (* the rows thing removes the final empty line and adds empty first line so we need to reset *)
                @ [""] 
            | [] -> [] (* *)
    } in
    match removed.sum with
        | 0 -> removed
        | x -> let next_iter = remove_possible removed.rows in
            {
                sum = removed.sum + next_iter.sum;
                rows = next_iter.rows;
            }
;;
let removed = remove_possible lines;;
let _ = printf "part 2: %d\n" removed.sum;;

