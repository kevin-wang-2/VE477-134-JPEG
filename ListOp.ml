let rec total arr =
  match arr with
  | [] -> 0
  | h::t -> h + total t
