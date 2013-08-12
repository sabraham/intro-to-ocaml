type number = Zero | Integer of int | Real of float

let float_of_number = function
Zero -> 0.0
  | Integer i -> float_of_int i
  | Real x -> x

type 'a btree = Node of 'a * 'a btree * 'a btree | Leaf

let rec cardinality = function
Leaf -> 0
  | Node (_, l, r) -> 1 + cardinality l + cardinality r

let empty = Leaf
let insert x s = Node (x, Leaf, s)
let rec set_of_list = function
[]   -> empty
  | x::l -> insert x (set_of_list l)

let s = set_of_list [3; 5; 7; 11; 13]

let rec mem x = function
Leaf -> false
  | Node (y, l, r) -> x = y || mem x l || mem x r

let rec insert x = function
Leaf -> Node (x, Leaf, Leaf)
  | Node (y, l, r) as node ->
    if x < y then
      Node (y, insert x l, r)
    else if x > y then
      Node (y, l, insert x r)
    else
      node

let s = set_of_list [7; 5; 9; 11; 3]

(* red-black trees *)

type color = Red | Black

type 'a rbtree = Leaf | Node of color * 'a * 'a rbtree * 'a rbtree

let rec mem x = function
Leaf -> false
  | Node (_, y, l, r) ->
    x = y || (x < y && mem x l) || (x > y && mem x r)

let balance = function
Black, z, Node (Red, y, Node (Red, x, a, b), c), d
  | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
  | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
  | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
    Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | a, b, c, d -> Node (a, b, c, d)

let insert x s =
  let rec ins = function
  Leaf -> Node (Red, x, Leaf, Leaf)
    | Node (col, y, a, b) as s ->
      if x < y then balance (col, y, ins a, b)
      else if x > y then balance (col, y, a, ins b)
      else s
  in match ins s with
      Node (_, y, a, b) -> Node (Black, y, a, b)
    | Leaf -> raise (Invalid_argument "insert")

let empty = Leaf

let rec set_of_list = function
[] -> empty
  | x::xs -> insert x (set_of_list xs)

let s = set_of_list [3; 9; 5; 7; 11]

let string_of_number1 (n : [`Integer of int | `Real of float]) =
  match n with
      `Integer i -> string_of_int i
    | _ -> raise (Invalid_argument "unknown number")

let string_of_number2 n =
  match n with
      `Real x -> string_of_float x
    | _ -> string_of_number1 n

let i = `Blah 5.0
in string_of_number2 i

let foo : 'a -> 'a =
  function x -> x

(* pretending that ocaml has typeclasses *)

(* let foo : Num 'a => 'a -> 'a = *)
(*   function x -> x * xggGGg *)

type number = [> `Integer of int | `Real of float]

(* Exercise 6.1 *)
type 'a mylist = Nil | Cons of 'a * 'a mylist

let rec map (f : 'a -> 'b) (l : 'a mylist) = match l with
    Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

(* Exercise 6.2 *)
let rec append (l1 : 'a mylist) (l2 : 'a mylist) = match l1 with
    Nil -> l2
  | Cons (x, xs) -> Cons (x, append xs l2)

(* Exercise 6.2 *)
type unary_number = Z | S of unary_number

let rec add_unary u1 u2 = match u1 with
    Z -> u2
  | S (xs) -> S (add_unary xs u2)

let mul_unary u1 u2 =
  let rec mul u1 u2 acc = match u2 with
      Z -> acc
    | S (xs) -> mul u1 xs (add_unary u1 acc)
  in mul u1 u2 Z

(* Exercise 6.3 *)
type small = Four | Three | Two | One

let lt_small sm1 sm2 = sm1 > sm2 (* cheating :p *)

(* Exercise 6.4 *)

type unop = Neg
type binop = Add | Sub | Mul | Div
type exp =
    Const of int
  | Unary of unop * exp
  | Binary of exp * binop * exp

let eval = function
Const (i) -> i
  | Unary (op, e) -> if op = Neg then -1 * eval e else eval e
  | Binary (e1, op, e2) -> match op with
      | Add -> (eval e1) + (eval e2)
      | Sub -> (eval e1) - (eval e2)
      | Mul -> (eval e1) * (eval e2)
      | Div -> (eval e1) / (eval e2)

(* Exercise 6.5 *)
type ('key, 'value) dtree = Leaf | Node of 'key * 'value * ('key, 'value) dtree * ('key, 'value) dtree


exception Empty
let empty : ('key, 'value) dtree = Leaf
let rec add (d : ('k, 'v) dtree) (k : 'k) (v : 'v) =   match d with
    Leaf -> Node(k, v, Leaf, Leaf)
  | Node(x, y, l, r) ->
    let c = compare k x
    in if c < 0 then Node(x, y, add l k v, r)
      else if c > 0 then Node(x, y, l, add r k v)
      else Node(x, v, l, r)
let rec find (d : ('k, 'v) dtree) (k : 'k) = match d with
    Leaf -> raise Empty
  | Node(x, v, l, r) ->
    let c = compare k x in
    if c < 0 then find l k
    else if c > 0 then find r k
    else v

(* Exercise 6.6 *)
type vertex = int
type graph = (vertex, vertex list) dtree

let reachable (g : graph) (v1 : vertex) (v2 : vertex) =
  let filter_graph g u v =
    add g u (List.filter (fun x -> x <> v) (find g u)) in
  let rec reachable' g u =
      if u = v2 then true
      else try
             match find g u with
                 [] -> false
               | vxs -> let gs = List.map (fun x -> filter_graph g u x) vxs in
                       let paths = List.map2 reachable' gs vxs in
                       List.fold_left (fun x y -> x || y) false paths
        with Empty -> false in
  if v1 <> v2
  then reachable' g v1
  else try
         List.exists (fun x -> x = v2) (find g v1)
    with Empty -> false;;

(* test *)
let gg = (add (add (add (add empty 1 [2; 3; 4]) 2 [4]) 3 [4]) 4 [5]);;
reachable gg 2 5;;
