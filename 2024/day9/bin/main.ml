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


let rec fill arr pos last =
  if pos = last then
    ()
  else if last >= Array.length !arr then
    ()
  else if !arr.(last) <> -1 then (
    !arr.(pos) <- !arr.(last);
    !arr.(last) <- -1)
  else
    fill arr pos (last - 1)


let compress arr =
  let r = ref arr in
  let last = Array.length arr - 1 in
  Array.iteri arr ~f:(fun i a ->
      match a with
      | -1 -> fill r i last
      | _ -> ());
  !r


let rec find_next_block arr last =
  let rec aux n l =
    if arr.(n) <> arr.(n + 1) then
      (n + 1, l)
    else
      aux (n - 1) (l + 1)
  in

  if arr.(last) <> -1 then
    aux (last - 1) 1
  else
    find_next_block arr (last - 1)


let find_space arr len =
  let start = ref 0 in
  let i = ref 0 in

  while !i - !start < len && !i < Array.length arr - 1 do
    if arr.(!i) <> -1 then (
      i := !i + 1;
      start := !i)
    else
      i := !i + 1
  done;

  if !i - !start >= len then
    Some !start
  else
    None


let compress_blocks arr =
  let r = ref arr in
  let i = Array.length arr - 1 |> Ref.create in

  while !i > 1 do
    let start, len = find_next_block !r !i in
    match find_space !r len with
    | Some empty_start when empty_start < start ->
        for i = 0 to len - 1 do
          Array.swap !r (empty_start + i) (start + i)
        done;
        i := start - 1
    | Some _ -> i := start - 1
    | _ -> i := start - 1
  done;
  !r


let check_sum =
  Array.foldi ~init:0 ~f:(fun i acc a ->
      if a = -1 then
        acc
      else
        acc + (i * a))


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
  |> Sequence.concat |> Sequence.to_array


let () =
  let input = InputReader.read_lines ~path:"input" ~f:Fn.id |> List.hd_exn in
  (*let input = "2333133121414131402" in*)
  input |> unfold_files |> compress |> check_sum |> Stdio.printf "Part1: %i\n";
  input |> unfold_files |> compress_blocks |> check_sum |> Stdio.printf "Part2: %i\n";
