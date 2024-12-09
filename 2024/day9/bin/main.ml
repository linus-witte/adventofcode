open Base

(*let print_files seq =*)
(*  Sequence.iter*)
(*    ~f:(fun n ->*)
(*      let c =*)
(*        if n >= 0 then*)
(*          Int.to_string n*)
(*        else*)
(*          "."*)
(*      in*)
(*      Stdio.printf "%s " c)*)
(*    seq;*)
(*  Stdio.printf "\n"*)

let int_at_opt s i =
  if i >= 0 && i < String.length s then
    Some (String.get s i |> String.of_char |> Int.of_string)
  else
    None


let pop_last seq =
  let rec pop seq acc =
    match Sequence.hd seq with
    | Some hd -> (
        let tail = Sequence.tl_eagerly_exn seq in
        match Sequence.hd tail with
        | Some _ -> pop tail (hd :: acc)
        | None -> (hd, Sequence.of_list (List.rev acc)))
    | None -> failwith "Empty sequence"
  in
  pop seq []


let compress seq =
  Sequence.unfold ~init:seq ~f:(fun seq ->
      match Sequence.hd seq with
      | Some hd when hd <> -1 -> Some (hd, Sequence.tl_eagerly_exn seq)
      | Some _ ->
          let r = pop_last seq |> ref in

          while fst !r = -1 do
            if snd !r |> Sequence.is_empty then
              r := (0, Sequence.singleton 0)
            else
              r := pop_last (snd !r)
          done;

          let x, s = !r in
          Some (x, Sequence.tl_eagerly_exn s)
      | _ -> None)


let check_sum =
  let open Z in
  Sequence.foldi ~init:zero ~f:(fun i acc a -> acc + (of_int i * of_int a))


let unfold_files files =
  Sequence.unfold ~init:0 ~f:(fun i ->
      int_at_opt files i
      |> Option.map ~f:(fun n ->
             let block =
               if i % 2 = 0 then
                 Sequence.init n ~f:(fun _ -> i / 2)
               else
                 Sequence.init n ~f:(fun _ -> -1)
             in
             (block, i + 1)))
  |> Sequence.concat


let () =
  let input = InputReader.read_lines ~path:"input" ~f:Fn.id |> List.hd_exn in

  (*let input = "233313312141413140210" in*)
  input |> unfold_files |> compress |> check_sum |> Z.to_string |> Stdio.printf "Part1: %s\n"
