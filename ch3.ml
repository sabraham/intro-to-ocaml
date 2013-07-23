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
