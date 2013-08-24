exception Fail of string;;
let head = function
    h::_ -> h
  | [] -> raise (Fail "head: the list is empty");;
head [3; 5; 6];;
head [];;

let head_default l default =
  try head l with
      Fail _ -> default;;

head_default [3; 5; 7] 0;;
head_default [] 0;;

exception Empty;;
let split = function
    h::t -> h, t
  | [] -> raise Empty;;

let rec map f l =
  try
    let h, t = split l in
    f h :: map f t
  with
      Empty -> [];;


exception Unchanged;;
let remove  x l =
  let rec remove' x = function
      y::l when x = y -> l
    | y::l (* x not = y *) -> y::remove' x l
    | [] -> raise Unchanged
  in try remove' x l with
      Unchanged -> l;;

let cat in_ch out_ch =
  try
    while true do
      output_char out_ch (input_char in_ch)
    done
  with
      End_of_file -> ();;

type 'a result = Success of 'a | Failed of exn;;

let finally f x cleanup =
  let result =
    try Success (f x) with
        exn -> Failed exn in
  cleanup();
  match result with
      Success y -> y
    | Failed exn -> raise exn;;

let process in_ch = 1; ();;
let process_file file_name =
  let in_ch = open_in file_name in
  finally process in_ch (fun () -> close_in in_ch);;

(* Exercise 9.1 *)
exception A;; (* valid *)
exception b;; (* invalid -- constructors need to be uppercase *)
exception C of string;; (* valid *)
exception D of exn;; (* valid *)
exception E of exn
let x = E(E (E Not_found));; (* valid -- E Not found = exn *)
let f () = exception F raise F;; (* invalid *)

(* Exercise 9.2 *)
exception A
try raise A with
    A -> 1;; (* 1 *)

exception A of int
let f i =
  raise (A (100 / i));;
let g i =
  try f i with
      A j -> j;;
g 100;; (* 1 *)

exception A of int
let rec f i =
  if i = 0 then
    raise (A i)
  else
    g (i - 1)
and g i =
  try f i with
      A i -> i + 1;;
g 2;; (* 1 *)

(* Exercise 9.3 *)
let table = [("a", 10); ("b", 20); ("c", 25)]
let rec sum_entries total (names : string list) =
  match names with
      name::names' -> sum_entries (total + List.assoc name table) names'
    | [] -> total;;

(* first form builds stack of exception handlers, second one does not. second is preferable. *)

(* Exercise 9.4 *)
let callf f name =
  try f (List.assoc table name) with
      Not_found -> 0;;

let callf' f name =
  let i =
    try Some (List.assoc table name) with
        Not_found -> None
  in match i with
      Some j -> f j
    | None -> 0;;

(* first form is problematic as f may catch the exception *)

(* Exercise 9.5 *)
let input_lines : string list =
  let ret = ref [] in
  try
    while true do
      ret := (input_line stdin)::!ret;
    done;
    !ret;
  with
      End_of_file -> !ret;;
