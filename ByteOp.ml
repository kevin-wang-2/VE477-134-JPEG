let rec combine_helper acc arr = 
  match arr with
  | [] -> acc
  | h::t -> combine_helper (h + acc * 0x100) t;;
  
let combine = combine_helper 0;;

let combine_and_next arr next = next (combine arr);;