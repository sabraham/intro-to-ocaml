(* use the fold Luke *)
let rec map f = function
  | [] -> []
  | x :: l -> f x :: map f l

let rec get key = function
  | (k, v) :: xs ->
    if k = key then
      v
    else
      get key xs
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
("ABC", (1, "def"), ()) (* 3 elements *)

(* 2 elements -- 'in' essentially creates a new scope and the parser
 *               allows us to drop parens on occassion => confusing code.
 *               The following are equivalent *)
(let x = 1 in x + 1, let y = 2 in y + 1, 4)
let x = 1 in (x+1, let y = 2 in y+1, 4)
let x = 1 in (x+1, let y = 2 in (y+1,4))

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

let database = ["John", "x3456", 50.1;
                "Jane", "x1234", 107.3;
                "Joan", "unlisted", 12.7]

exception Empty

let rec find_salary (name : string) (db: (string*string*float) list) : float =
  match db with
    [] -> raise Empty
  | (n, phone, salary)::xs ->
    if name = n then salary else find_salary name xs

let select
    (pred : string * string * float -> bool) (db: (string*string*float) list)
    : (string*string*float) list =
  let rec aux acc rest =
    match rest with
        [] -> acc
      | x::xs ->
        if pred x then
	  aux (x::acc) xs
        else
	  aux acc xs
  in aux db []

(* Exercise 5.7 *)

let rev_concat l1 l2 =
  let rec aux l1 acc =
    match l1 with
        [] -> acc
      | x::xs -> aux xs (x::acc)
  in List.rev (aux l1 (List.rev l2))

(* find a string in the intersection of welfare, actors, residents, which
   are all sorted lists of strings*)
exception Mystery

let rec find_crook
    (welfare : string list) (actors : string list) (residents : string list)
    : string =
  match welfare, actors, residents with
      [], _, _ | _, [], _ | _, _, [] -> raise Empty
    | w::ws, b::bs, r::rs ->
      if w = b && w = r then
	w
      else
	let m = min (min w b) r in
           match w, b, r with
             x, _, _ when x = m -> find_crook ws actors residents
           | _, x, _ when x = m -> find_crook welfare bs residents
           | _, _, x when x = m -> find_crook welfare actors rs
           | _, _, _            -> raise Mystery


(* Code Review Notes
 * - maybe add a few more comments
 * - better function names would be nice
 * - I didn't look at the problems until just, so some of my suggestions may
 *   not actually jive with the book, find_salary in particular
 * - find_crook is a little ugly, but I couldn't find a way to clean it up
 * - looks good for the most part
**)
