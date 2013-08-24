open Printf;;
printf "///%8.3f///" 3.1415926;;

open Scanf;;
sscanf "ABC 345" "%s %d" (fun s i -> s, i);;

(* Exercise 10.1 *)
printf "Hello world!";;

(* Exercise 10.2 *)
let read_lines chan =
  let rec loop lines =
    let l =
      (try Some (input_line chan) with
          End_of_file -> None) in
    match l with
        Some x -> loop (x::lines)
      | None -> List.rev lines in
  loop [];;
