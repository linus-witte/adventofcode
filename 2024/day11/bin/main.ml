open Base

let digitCount i = i |> Int.to_string |> String.length

let split i =
  let s = i |> Int.to_string in
  let n = String.length s in
  ( String.unsafe_sub s ~pos:0 ~len:(n / 2) |> Int.of_string,
    String.unsafe_sub s ~pos:(n / 2) ~len:(n / 2) |> Int.of_string )


let transform_n lst n =
  let table = ref (Stdlib.Hashtbl.create 16) in

  let rec aux x n =
    if Stdlib.Hashtbl.mem !table (x, n) then
      Stdlib.Hashtbl.find !table (x, n)
    else if n = 0 then
      1
    else
      let res =
        match x with
        | 0 -> aux 1 (n - 1)
        | m when digitCount m % 2 = 1 -> aux (m * 2024) (n - 1)
        | m ->
            let l, r = split m in
            aux l (n - 1) + aux r (n - 1)
      in
      Stdlib.Hashtbl.add !table (x, n) res;
      res
  in

  List.fold lst ~init:0 ~f:(fun acc a -> acc + aux a n)


let () =
  let input =
    InputReader.read_lines ~path:"input" ~f:(String.split ~on:' ') |> List.hd_exn |> List.map ~f:Int.of_string
  in

  transform_n input 25 |> Stdio.printf "Part1: %i\n";
  transform_n input 75 |> Stdio.printf "Part2: %i\n"
