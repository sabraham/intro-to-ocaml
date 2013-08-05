let rec map f = function
  | [] -> []
  | x :: l -> f x :: map f l

let rec assoc key = function
  | (key2, value) :: l ->
    if key2 = key then
      value
    else
      assoc key l
  | [] -> raise Not_found;;

let rec rev accum = function
  | h :: t -> rev (h :: accum) t
  | [] -> accum

let rec rev_map f accum = function
  | h :: t -> rev_map f (f h :: accum) t
  | [] -> accum

let map f l = rev [] (rev_map f [] l)

(* Exercise 5.1 *)

(1 + 2, 3, - 5) (* 3 elements *)
"ABC", (1, "def"), ()) (* 3 elements *)
(let x = 1 in x + 1, let y = 2 in y + 1, 4) (* 2 elements -- ??? *)

(* Exercise 5.2 *)
let f (x, y, z, w) = x + z (* f : int * 'a * int * 'b * -> int *)
let f (x, y, z, w) = (w, z, y, x) (* f : 'a * 'b * 'c * ''d -> 'd * 'c * 'b * 'a *)
let f [x; y; z; w] = x (* f : 'a list -> 'a *)
let f [x; y] [z; w] = [x; z] (* f : 'a list -> 'a list -> 'a list *)
let f (x, y) (z, w) = [x; z] (* f : 'a * 'b -> 'a * 'c -> 'a list *)
let nth i (x, y, z) =
  match i with
      1 -> x
    | 2 -> y
    | 3 -> z
    | _ -> raise (Invalid_argument "nth")

      (* type: nth int -> 'a * 'a * 'a *)

let db = ["John", "x3456", 50.1;
          "Jane", "x1234", 107.3;
          "Joan", "unlisted", 12.7]

exception Empty

let find_salary (name : string) : float =
  let rec find_salary' db (name : string) =
    match db with
        [] -> raise Empty
      | (n, phone, salary)::xs ->
        if name = n then salary else find_salary' xs name
  in find_salary' db name

let select (pred : string * string * float -> bool) =
  let rec select' (db : ((string * string * float) list)) acc =
    match db with
        [] -> []
      | x::xs ->
        if pred x
        then select' xs (x::acc)
        else select' xs acc
  in select' db []

(* Exercise 5.7 *)

let append_tr l1 l2 =
  let rec append_tr' l1 acc =
    match l1 with
        [] -> acc
      | x::xs -> append_tr' xs (x::acc)
  in List.rev (append_tr' l1 (List.rev l2))

(* find a string in the intersection of welfare, actors, residents, which
   are all sorted lists of strings*)
exception Mystery
let rec find_crook
    (welfare : string list) (actors : string list) (residents : string list) =
  match welfare, actors, residents with
      [], _, _ | _, [], _ | _, _, [] -> raise Empty
    | w::ws, b::bs, r::rs ->
      if w = b && w = r
      then w
      else let m = min (min w b) r
           in match w, b, r with
               x, _, _ when x = m -> find_crook ws actors residents
             | _, x, _ when x = m -> find_crook welfare bs residents
             | _, _, x when x = m -> find_crook welfare actors rs
             | _, _, _ -> raise Mystery
