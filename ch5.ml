let rec map f = function
[] -> []
  | x :: l -> f x :: map f l

let rec assoc key = function
(key2, value) :: l ->
  if key2 = key then
    value
  else
    assoc key l
  | [] -> raise Not_found;;
