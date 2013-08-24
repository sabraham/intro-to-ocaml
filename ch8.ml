type db_entry = {
  name : string;
  height : float;
  phone : string;
  salary : float
};;

let jason = {
  name = "Jason";
  height = 6.25;
  phone = "626-555-1212";
  salary = 50.0
};;

let {name = n; height = h } = jason;;
let dave = {jason with name = "Dave"; height = 5.9};;
jason;;
dave;;

type db_entry = {
  name : string;
  height : float;
  phone : string;
  mutable salary : float
};;

let jason = {
  name = "Jason";
  height = 6.25;
  phone = "626-555-1212";
  salary = 50.0
};;

jason.salary <- 150.0;;
jason;;

let random_numbers = Array.map (fun x -> Random.bits()) [|1;2;3;4;5;6;7|];;
let random_length = Array.length random_numbers;;
type hash_info = {mutable hash_index : int; mutable hash_value : int};;

let hash_char info c =
  let i = Char.code c in
  let index = (info.hash_index + i + 1) mod random_length in
  info.hash_value <- (info.hash_value * 3) lxor random_numbers.(index);
  info.hash_index <- index;;

let hash s =
  let info = {hash_index = 0; hash_value = 0} in
  for i = 0 to String.length s - 1 do
    hash_char info s.[i]
  done;
  info.hash_value;;

type 'a hash_entry = {key : string; value : 'a };;
type 'a hash_table = 'a hash_entry list array;;

let create () : 'a hash_table =
  Array.create 101 [];;

let add (table : 'a hash_table) (key : string) (value : 'a) =
  let index = (hash key) mod (Array.length table) in
  table.(index) <- {key = key; value = value} :: table.(index);;

let rec find_entry l key = match l with
    {key = key'; value = value} :: _ when key' = key -> value
  | _ :: entries -> find_entry entries key
  | [] -> raise Not_found;;

let find d key =
  let index = (hash key) mod (Array.length d) in
  find_entry d.(index) key;;

(* Exercise 8.1 *)
type 'a ref = {mutable contents : 'a};;

let ref (x : 'a) : 'a ref = {contents = x};;
let (!) (x : 'a ref) : 'a = x.contents;;
let (:=) (x : 'a ref) (x' : 'a) : unit = x.contents <- x';;

(* Exercise 8.2 *)
type ('a, 'b) mpair = {mutable fst : 'a; snd : 'b};;
[|[]|];; (* '_a list array *)
{ fst = []; snd = []};; (* ('_a list, '_b list) mpair *)
{{fst = (); snd = 2} with fst = 1};; (* (int, int) mpair *)

(* Exercise 8.3 *)
type ('key, 'value) dictionary = {
  insert : 'key -> 'value -> ('key, 'value) dictionary;
  find : 'key -> 'value
};;

exception Empty;;
let empty : ('key, 'value) dictionary =
  let find (k : 'key) = raise Empty in
  let rec insert (k : 'key) (v : 'value) =
    {insert = insert;
     find = (fun (k' : 'key) -> if k' = k then v else find k')} in
  {insert = insert;
   find = find};;

(* Exercise 8.4 *)
type blob = {
  get  : unit -> float * float;
  area : unit -> float;
  set  : float * float -> unit;
  move : float * float -> unit
};;

let new_rectangle x y w h =
  let pos = ref (x,y) in
  let rec r = {
    get = (fun () -> !pos);
    area = (fun () -> w *. h);
    set = (fun loc -> pos := loc);
    move = (fun (dx, dy) -> let (x, y) = r.get() in
                           r.set (x +. dx, y +. dy))
  } in
  r;;

let rect1 = new_rectangle 0.0 0.0 1.0 1.0;;
rect1.move (1.3, 3.4);;
let rect2 = {rect1 with set = (fun _ -> ())};;
rect2.move(3.0, 4.0);;
rect2.get();; (* rect2 changed *)
let rect2 = {rect1 with set = (fun _ -> ()); move = (fun _ -> ())};;
rect2.move (3.0, 4.0);;
rect2.get();; (* rect2 did not change *)
rect1.set(3.0, 4.0);;
rect2.get();; (* rect2 changed *)

(* Exercise 8.5 *)
let string_reverse (s : string) : unit =
  let l = String.length s - 1 in
  let cxor x y = Char.chr ((Char.code x) lxor (Char.code y)) in
  let cswap s i j =
    s.[i] <- cxor s.[i] s.[j];
    s.[j] <- cxor s.[i] s.[j];
    s.[i] <- cxor s.[i] s.[j]; in
  for i = 0 to (l / 2) do
    cswap s i (l - i)
  done;;
