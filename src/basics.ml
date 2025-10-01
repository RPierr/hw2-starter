let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)


let rev_tup (tup : 'a * 'b) = 
  match tup with 
  | (a,b) -> (b,a)

let rev_triple (tup : 'a * 'b * 'c) = 
  match tup with
  | (a,b,c) -> (c,b,a)

let is_odd x =
  x mod 2 <> 0

let is_older (year1, month1, day1) (year2, month2, day2) = 
  (year1, month1, day1) < (year2, month2, day2)

let to_us_format (date1: int * int * int) = 
  let (y,m, d) = date1 in
  (m, d, y)


(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p = 
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac n =
  if n = 1 then 1
  else n * fac (n-1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth (idx, lst) = 
  match (idx, lst) with
  | (0, x :: _) ->  x
  | (n, _ :: xs ) when n > 0 -> get_nth (n - 1, xs)
  | _ -> failwith "Index out of bounds"

let larger lst1 lst2 = 
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 > len2 then lst1
  else if len2 > len1 then lst2
  else []

let sum lst1 lst2 = 
  let rec sumList lst =
    match lst with
    | [] -> 0
    | x :: xs -> x + sumList xs
  in
  sumList lst1 + sumList lst2

