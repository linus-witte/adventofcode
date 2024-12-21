open Base
open Util
module Hashtbl = Stdlib.Hashtbl

let len_match l1 l2 =
  let rec aux n = function
    | hd1 :: tl1, hd2 :: tl2 when Char.(hd1 = hd2) -> aux (n + 1) (tl1, tl2)
    | _ -> n
  in
  aux 0 (l1, l2)


let rec try_match table avail pattern : int =
  if Hashtbl.mem !table pattern then
    Hashtbl.find !table pattern
  else
    let res =
      List.map avail ~f:(fun p ->
          let n = len_match pattern p in
          if n = List.length p then
            let remaining_pattern = List.drop pattern n in
            if List.is_empty remaining_pattern then
              1
            else
              try_match table avail remaining_pattern
          else
            0)
      |> List.fold ~init:0 ~f:Int.( + )
    in
    Hashtbl.add !table pattern res;
    res


let () =
  let available =
    InputReader.read_lines_filter ~path:"input" ~f:(fun line ->
        if String.mem line ',' then
          Some
            (String.substr_replace_all ~pattern:" " ~with_:"" line
            |> String.split ~on:',' |> List.map ~f:String.to_list)
        else
          None)
    |> List.hd_exn
  in
  let desired =
    InputReader.read_lines_filter ~path:"input" ~f:(fun line ->
        if (not @@ String.mem line ',') && (not @@ String.is_empty line) then
          Some (String.substr_replace_all ~pattern:" " ~with_:"" line |> String.to_list)
        else
          None)
  in

  let table = ref (Hashtbl.create 16) in
  List.count desired ~f:(fun pattern -> try_match table available pattern > 0) |> Stdio.printf "Part 1: %i\n";
  List.fold desired ~init:0 ~f:(fun acc pattern -> acc + try_match table available pattern)
  |> Stdio.printf "Part 2: %i\n"
