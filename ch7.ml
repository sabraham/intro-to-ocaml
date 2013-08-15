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

(* Problems *)

let x = ref 1 in
let y = x in
y := 2;
!x;; (* 2 *)

let x = ref 1 in
let y = ref 1 in
y := 2;; (* () *)

let x = ref 1 in
let y = ref x in
!y := 2;
!x;; (* 2 *)

let fst (x, _) = x in
let snd (_, x) = x in
let y = ref 1 in
let x = (y, y) in
fst x := 2;
!(snd x);; (* 2 *)

let x = ref 0 in
let y = ref [5; 7; 2; 6] in
while !y <> [] do
  x := !x + 1;
  y := List.tl !y
done;
!x;;

(* Exercise 7.2 *)

type 'a deferred = (unit -> 'a) * ('a option ref) ;;
let defer (f : unit -> 'a) : 'a deferred = (f, ref None);;
let force (d : 'a deferred) : 'a =
  let (f, r) = d in
  match !r with
      None -> let res = f () in
             r := Some res;
             res
    | Some res -> res

(* Exercise 7.3 *)

type 'a lazy_list =
    Nil
  | Cons of 'a * 'a lazy_list
  | LazyCons of 'a * 'a lazy_list deferred;;

let nil = Nil;;
let cons (x : 'a) (l : 'a lazy_list) = Cons (x, l);;
let lazy_cons (x : 'a) (l : unit -> 'a lazy_list) = LazyCons(x, l);;
let is_nil (x : 'a) : bool = nil = x;;
let head = function
    Nil -> raise Empty
  | Cons(x, _) -> x
  | LazyCons(x, _) -> x;;
let tail = function
    Nil -> raise Empty
  | Cons(_, xs) -> xs
  | LazyCons(_, xs) -> force xs;;
let rec (@@) (l1 : 'a lazy_list) (l2 : 'a lazy_list) = match l1 with
    Nil -> l2
  | Cons(x, xs) -> Cons(x, xs @@ l2)
  | LazyCons(x, xs) -> let r = (force xs) @@ l2 in
                      LazyCons(x, defer (fun () -> r));;

(* Exercise 7.4 *)
type 'a queue = 'a lazy_list * 'a lazy_list;;
let rev (l : 'a lazy_list) : 'a lazy_list =
  let rec rev' l acc = match l with
      Nil -> Nil
    | Cons(x, xs) -> rev' xs (Cons(x, acc))
    | LazyCons(x, xs) -> rev' (force xs) (LazyCons(x, defer (fun () -> acc))) in
  rev' l Nil;;

let empty : 'a queue = (Nil, Nil);;
let add (q : 'a queue) (x : 'a) : 'a queue =
  let (f, b) = q in (f, Cons(x, b));;
let take (q : 'a queue) = match q with
    Cons(x, xs), b -> (x, (xs, b))
  | LazyCons(x, xs), b -> (x, (force xs, b))
  |  Nil, b -> match b with
        Nil -> raise Empty
      | Cons(x, xs) -> (x, (rev xs, Nil))
      | LazyCons(x, xs) -> (x, (rev (force xs), Nil));;

(* Exercise 7.5 *)
type ('a, 'b) memo = ('a * 'b) list ref;;
let create_memo : unit -> ('a, 'b) memo = (fun () -> ref []);;
let memo_find (m : ('a, 'b) memo) (k : 'a) =
  let rec find' arr k = match arr with
    [] -> raise Empty
  | (x, v)::xs ->
    if k = x then v
    else find' xs k
  in find' !m k;;
let memo_add (m : ('a, 'b) memo) (k : 'a) (v : 'b) =
  m := (k, v)::!m;;
let rec memo_fib (m : (int, int) memo) (k : int) : int = match k with
    0 | 1 -> 1
  | _ ->
    try
      memo_find m k
    with Empty ->
      let v = (memo_fib m (k - 1)) + (memo_fib m (k - 2)) in
      memo_add m k v;
      v;;

let fib = memo_fib(create_memo ());;

(* Exercise 7.5 - Graph DFS *)
type 'a vertex = Vertex of 'a * 'a vertex list ref * bool ref * int option ref;;
type 'a edge = Edge of 'a vertex * 'a vertex;;
type 'a directed_graph = 'a vertex list;;
type 'a vertex_class = TreeE | ForwardE | BackE | CrossE;;
let classify (u : 'a vertex) (v : 'a vertex) : 'a vertex_class =
  let Vertex(_, _, v_mark, v_ind) = v in
  if !v_ind = None then TreeE
  else let Vertex(_, _, _, u_ind) = u in
       if u_ind < v_ind then ForwardE
       else match !v_mark with
           false -> BackE
         | true -> CrossE;;

let edge_list (v : 'a vertex) (out : 'a vertex list) : 'a edge list=
  List.map (fun x -> Edge(v, x)) out;;

let dfs (l : 'a vertex list)  =
  let rec dfs' (s : 'a edge list) (c : int ref) =
    match s with
        [] -> raise Empty
      | Edge((Vertex(_, _, u_mark, u_ind) as u),
             (Vertex(_, out_vs, _, v_ind) as v))::es ->
        let eclass = classify u v in
        if eclass = TreeE then
          v_ind := Some !c;
          c := !c + 1;
          (match es with
              Edge(x, y)::es' when x <> u -> u_mark := true
            | _ -> ());
          dfs' (es @ (edge_list v !out_vs)) c
  in match l with
      [] -> raise Empty
    | (Vertex(_, out, _, u_ind) as u)::us ->
      let c = ref 0 in
      u_ind := Some !c;
      c := !c + 1;
      dfs' (edge_list u !out) c;;

let test_d = Vertex("d", ref [], ref false, ref None);;
let test_c = Vertex("c", ref [test_d], ref false, ref None);;
let test_b = Vertex("b", ref [test_d], ref false, ref None);;
let test_a = Vertex("a", ref [test_b; test_c], ref false, ref None);;
let test_vs = [test_a; test_b; test_c; test_d];;
dfs test_vs;;
