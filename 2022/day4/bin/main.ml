open InputReader
open Stdio
open Base

let transform line = 
    let res = String.split ~on:',' line in
    match res with
    | e1 :: e2 :: [] -> 
        let list = List.map res ~f:(fun s -> 
            let list = String.split s ~on:'-' in
            let (l, r) = (Int.of_string (List.hd_exn list), Int.of_string (List.last_exn list)) in
            List.init (r-l + 1) ~f:(fun x -> l + x)) in
        (List.hd_exn list, List.last_exn list)
    | _ -> failwith "invalid input"

let rec list_to_string list = 
    match list with
    | [] -> ""
    | (hd::tl) -> (Int.to_string hd) ^ (list_to_string tl)

let part1 = 
    let input = InputReader.read_lines ~path:"input" ~f:transform in
    let inter = List.filter input ~f:(fun (e1, e2) -> 
        let s1 = Set.of_list (module Int) e1 in
        let s2 = Set.of_list (module Int) e2 in
        Set.is_subset s1 ~of_:s2 || Set.is_subset s2 ~of_:s1) in
    Stdio.printf "Part1: %i\n" (List.length inter)
    
let part2 = 
    let input = InputReader.read_lines ~path:"input" ~f:transform in
    let inter = List.filter input ~f:(
        fun (e1, e2) -> 
        let s1 = Set.of_list (module Int) e1 in
        let s2 = Set.of_list (module Int) e2 in
    (>) (Set.length (Set.inter s1 s2)) 0) in
Stdio.printf "Part2: %i\n" (List.length inter)

let () =
    part1;
    part2
