open InputReader
open Base 

let inter s = 
    let (s1, s2) = s in
    let set1 = Set.of_list (module Char) (String.to_list s1) in
    let set2 = Set.of_list (module Char) (String.to_list s2) in
    Set.to_list (Set.inter set1 set2)

let priority c =
    match Char.is_uppercase c with
    | true -> Char.to_int c - 65 + 27
    | false -> Char.to_int c - 96

let part1 = 
    let transformation x = 
        let n = String.length x / 2 in
        (String.prefix x n, String.suffix x n) in

    let input = InputReader.read_lines ~path:"input" ~f:(fun x -> (transformation x) |> inter) in
    List.fold_right input ~f:(fun x xs -> match x with (hd::_) -> priority hd + xs | _ -> failwith "Error in input") ~init:0

let part2 = 
    let input = InputReader.read_lines ~path:"input" ~f:(fun x -> x) in
    let rec group list =
        match list with
        | a::b::c::tl -> (a,b,c) :: group tl
        | _ -> [] in
    let groups = group input in
    let intersection (a,b,c) = 
        let list = inter (String.of_list (inter (a,b)), c) in
        match list with
        | hd::_ -> hd
        | _ -> failwith "Error in input" in

    List.fold_right groups ~f:(fun x xs -> priority (intersection x) + xs) ~init:0

let () = 
    Stdio.printf "Part 1: %i\n" part1;
    Stdio.printf "Part 2: %i\n" part2
    
