open Base

let () =
  let derivative input = List.map input ~f:(fun lst -> Sequence.unfold ~init:lst ~f:(function a :: b :: rest -> Some ((b - a), b :: rest) | _ -> None) |> Sequence.to_list) in

  let save lst = 
    let gt = List.for_all lst ~f:(Int.(>) 0) in
    let ls = List.for_all lst ~f:(Int.(<) 0) in

    if (Bool.((gt || ls) = false)) then false else
      List.for_all lst ~f:(fun i -> Int.abs i |> Int.(>) 4) in

  let input = InputReader.read_lines ~path:"input" ~f:(fun s -> String.split s ~on:' ' |> fun l -> List.map l ~f:(Int.of_string)) in

  Stdio.printf "part 1: %i\n" (List.count (derivative input) ~f:save);

  let rec partials = function
    | [] -> []
    | (x :: xs) -> xs :: (List.map (partials xs) ~f:(fun l -> x :: l)) in

  let res = List.map input ~f:(fun d -> d :: partials d |> derivative) in
  Stdio.printf "part 2: %i\n" (List.count res ~f:(fun lst -> List.exists lst ~f:save))
