(* Exercise 3.3 *)

let sum n m f =
  let rec sum' n m acc =
    if n > m then acc
    else sum' (n + 1) m (acc + (f n))
  in sum' n m 0

(* Exercise 3.5 *)

let rec min_nonneg_val f n m =
  let mid = (m + n) / 2 in
  let midp1 = mid + 1 in
  match compare (f mid) 0, compare (f midp1) 0 with
      -1, 0 | -1, 1 -> midp1
    | -1, -1 -> min_nonneg_val f midp1 m
    |  _, _  -> min_nonneg_val f n mid

(* Exercise 3.6 - dict*)

let empty k = 0

let add dict k v =
  let dict' k' =
    if (k' = k) then v
    else dict k'
  in dict'

let find dict k = dict k

(* Exercise 3.8 - streams*)

type stream = int -> int
let hd (s : stream) = s 0
let tl (s : stream) = (fun i -> s (i + 1))

let (+:) (s : stream) (c : int) = (fun i -> s i + c)

let (-|) (s1 : stream) (s2 : stream) = (fun i -> s1 i - s2 i)

let map (f : int -> int) (s : stream) = (fun i -> f (s i))

let derivative (s : stream) = tl s -| s

let rec integral (s : stream) = fun i -> match i with
    0 -> 0
  | _ -> ((integral s) (i - 1)) + s i
