open Stdio

let inputFile = "input"

let read_input = 
    let ic = open_in inputFile in
    let rec next ic =
        try
            let line = String.split_on_char ' ' (input_line ic) in
            match line with
            | [a;b] -> (a,b) :: (next ic)
            | _ -> next ic
        with End_of_file -> 
            close_in ic;
            [] in
    next ic


let score_shape shape = 
    match shape with
    | "X" -> 1
    | "Y" -> 2
    | "Z" -> 3
    | _ -> 0

(*
A/X -> Rock
B/Y -> Paper
C/Z -> Scissors
*)

let result op u =
    match (op,u) with
    | ("A", "X") -> 3
    | ("A", "Y") -> 6
    | ("A", "Z") -> 0
    | ("B", "X") -> 0
    | ("B", "Y") -> 3
    | ("B", "Z") -> 6
    | ("C", "X") -> 6
    | ("C", "Y") -> 0
    | ("C", "Z") -> 3
    | _ -> 0

let transform (op,u) =
    match (op,u) with
    | ("A", "X") -> ("A", "Z")
    | ("A", "Y") -> ("A", "X")
    | ("A", "Z") -> ("A", "Y")
    | ("B", "X") -> ("B", "X")
    | ("B", "Y") -> ("B", "Y")
    | ("B", "Z") -> ("B", "Z")
    | ("C", "X") -> ("C", "Y")
    | ("C", "Y") -> ("C", "Z")
    | ("C", "Z") -> ("C", "X")
    | _ -> (op,u)


let rec score rounds =
    match rounds with
    | (op,u)::tail -> score_shape u + result op u + score tail
    | [] -> 0
    (*score_shape u + result op u*)

let () = 
    let input = read_input in
    printf "Total S1: %i\nTotal S2: %i\n" (score input) (score (List.map transform input))
