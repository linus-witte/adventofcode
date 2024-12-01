open Base

let () = 
  let input = InputReader.read_lines ~path:"input" ~f:(fun s -> match String.split s ~on:' ' with [l;r] -> (Int.of_string l, Int.of_string r) | _ -> failwith "") in

  let (l, r) = List.unzip input in

  let l = List.sort l ~compare:Int.compare in
  let r = List.sort r ~compare:Int.compare in

  let diff = Sequence.unfold ~init:(l,r) ~f:(fun x -> 
    match x with
    | (l :: ls, r :: rs) -> Some (Int.abs (l - r), (ls, rs))
    | _ -> None 
    ) in

  Stdio.printf "%i\n" (Sequence.fold diff ~init:0 ~f:(+))
