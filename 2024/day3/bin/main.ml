open Base

let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" 

let is_enabled input pos = 
  let last_enabled = try Str.search_backward (Str.regexp "do()") input pos with _ -> 0 in
  let last_disabled = try Str.search_backward (Str.regexp "don't()") input pos with _ -> -1 in
  last_enabled > last_disabled


let () =
  let input = InputReader.read_lines ~path:"input" ~f:(fun line -> line) |> String.concat in

  let lst = Sequence.unfold ~init:0 ~f:(fun pos -> 
    try
      let next = Str.search_forward regex input pos in
      let l = Str.matched_group 1 input |> Int.of_string in
      let r = Str.matched_group 2 input |> Int.of_string in
      Some ((l, r, is_enabled input next), next + 1)
    with | _ -> None
      ) |> Sequence.to_list in

  let lst = List.filter lst ~f:(fun (l,r,_) -> (l < 1000) && (r < 1000)) in

  List.fold lst ~init:0 ~f:(fun acc (l,r,_) -> acc + l * r) |> Stdio.printf "part1: %i\n";
  List.fold lst ~init:0 ~f:(fun acc (l,r,e) -> if e then acc + l * r else acc) |> Stdio.printf "part2: %i\n";
