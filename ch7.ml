let factorial i =
  let j = ref 1 in
  for k = 2 to i do
    j := !j * k
  done;
  !j

(* queue *)
type 'a queue = ('a list * 'a list) ref

let create () = ref ([], [])

let add (q : 'a queue) (x : 'a) =
  match !q with
      f, b -> q := (f, x::b);;

let foo = create ();;
add foo 1;;

exception Empty
let rec take (q : 'a queue) =
  match !q with
      [], [] -> raise Empty
    | x::xs, b -> q := (xs, b); x
    | [], b -> q := (List.rev b, []); take q

(* doubly-linked list *)

type 'a elem = Nil | Elem of 'a * 'a elem ref * 'a elem ref
let nil_elem = Nil
let create_elem (x : 'a) = Elem (x, ref Nil, ref Nil)
let get x = function
    Nil -> raise Empty
  | Elem (x, _, _) -> x
let prev_elem = function
    Nil -> raise Empty
  | Elem (_, p, _) -> p
let next_elem = function
    Nil -> raise Empty
  | Elem (_, _, n) -> n
type 'a dllist = 'a elem ref
let create () = ref Nil
let insert l elem =
  match elem, !l with
      Nil, _ -> raise Empty
    | Elem (_, p, n), Nil ->
        p := Nil;
        n := Nil;
        l := elem;
    | Elem (x, p1, n1), (Elem (_, p2, n2) as head)->
        p1 := Nil;
        n1 := head;
        p2 := elem;
        l := elem;;
let remove l elem =
  match elem with
      Nil -> raise Empty
    | Elem (_, p, n) ->
      (match !p with
          Nil -> l := !n
        | Elem (_, _, p_n) -> p_n := !n);
      (match !n with
          Nil -> ()
        | Elem (_, n_p, _) -> n_p := !p);;

(* Memoization *)
let memo f =
  let table = ref [] in
  let rec find_or_apply entries x =
    match entries with
        (x', y) ::_ when x' = x -> y
      | _::entries -> find_or_apply entries x
      | [] ->
        let y = f x in
        table := (x, y)::(!table);
        y
  in (fun x -> find_or_apply !table x)

let rec fib = function
    0 | 1 as i -> i
  | i -> fib (i - 1) + fib (i - 2);;

let memo_fib = memo fib;;

let time f x =
  let start = Sys.time() in
  let y = f x in
  let finish = Sys.time() in
  Printf.printf "Elapsed time: %f seconds]n" (finish -. start);
  y;;

(* Graphs *)
type 'a parent = Root of int | Parent of 'a vertex and
  'a vertex = 'a * 'a parent ref
type 'a edge = float * 'a vertex * 'a vertex;;

let union ((_, p1) as u1) ((_, p2) as u2) =
  match !p1, !p2 with
      Root size1, Root size2 when size1 > size2 ->
        p2 := Parent u1;
        p1 := Root (size1 + size2)
    | Root size1, Root size2 ->
        p1 := Parent u2;
        p2 := Root (size1 + size2)
    | _ -> raise (Invalid_argument "union: not roots");;

let rec compress root (_, p) =
  match !p with
      Root _ -> ()
    | Parent v -> p := Parent root; compress root v;;

let rec simple_find ((_, p) as v) =
  match !p with
      Root _ -> v
    | Parent v -> simple_find v;;

let find v =
  let root = simple_find v in
  compress root v;
  root;;

let kruskal (edges : 'a edge list) =
  let spanning_tree = ref [] in
  List.iter (fun ((_, v1, v2) as edge) ->
    let u1 = find v1 in
    let u2 = find v2 in
    if u1 != u2 then begin
      spanning_tree := edge::(!spanning_tree);
      union u1 u2
    end) edges;
  !spanning_tree;;
