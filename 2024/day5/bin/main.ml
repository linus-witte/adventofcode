open Base

let compare rules e1 e2 = 
  if Sequence.exists rules ~f:(fun (l,r) -> l = e1 && r = e2) then -1 
  else 
    if Sequence.exists rules ~f:(fun (l,r) -> l = e2 && r = e1) then 1 
    else 0

let median lst = List.length lst / 2 |> List.nth_exn lst

let () =
  let rules = InputReader.read_lines_filter ~path:"input" ~f:(fun line -> 
    match (String.split line ~on:'|') with
    | [l;r] -> Some (Int.of_string l, Int.of_string r)
    | _ -> None) |> Sequence.of_list in

  let updates = InputReader.read_lines_filter ~path:"input" ~f:(fun line ->
    match (String.split line ~on:',') with
    | _ :: _ :: _ as list -> Some (List.map list ~f:Int.of_string)
    | _ -> None) in

  let sum1 = List.filter_map updates ~f:(fun update -> 
    if List.is_sorted_strictly ~compare:(compare rules) update then
      Some (median update) else None) 
    |> List.fold ~init:0 ~f:(+) in

  let sum2 = List.map updates ~f:(fun lst -> List.sort lst ~compare:(compare rules) |> median) |> List.fold ~init:0 ~f:(+) in

  Stdio.printf "part1: %i\n" sum1;
  Stdio.printf "part2: %i\n" (sum2 - sum1)

