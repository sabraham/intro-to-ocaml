(* Exercise 4.1 *)

match 1 with
    1 -> 2
  | _ -> 3

match 2 with
    1 + 1 -> 2
  | _ -> 3

let _ as s = "abc" in s ^ "def"

(fun (1 | 2 as i) -> i + 1) 2

(* Exercise 4.3 *)

let check s1 s2 =
  let count = String.length s1 in
  let rec check' ind =
    if ind = count then true
    else match s1.[ind], s2.[ind] with
        'A', 'C' | 'B', 'A' | 'C', 'D' | 'D', 'B' -> check' (ind + 1)
      | ' ' .. '~', _ -> false
      | _, _ -> raise (Invalid_argument "check -- s1 not plaintext string")
  in check' 0
