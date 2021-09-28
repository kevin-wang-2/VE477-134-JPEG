let safe_open_bin filename fn = 
  let fd = open_in_bin filename in 
    try
      fn fd
    with e->
      close_in_noerr fd;
      raise e;;

let rec read_cnt_helper fd cnt arr fn next =
  match cnt with
  | 0 ->
    fn arr next
  | n -> read_cnt_helper fd (cnt - 1) (arr @ [(input_byte fd)]) fn next;;

let read_cnt fd cnt fn next = 
  read_cnt_helper fd cnt [] fn next;;
  
let rec safe_read_cnt_helper fd cnt arr fn next =
  try
    match cnt with
    | 0 -> fn arr next
    | n -> safe_read_cnt_helper fd (cnt - 1) (arr @ [(input_byte fd)]) fn next
  with e ->
    Logging.secure_open "test.log" output_string "EOF";
    close_in_noerr fd;
    fn arr next;;

let safe_read_cnt fd cnt fn next = 
  safe_read_cnt_helper fd cnt [] fn next;;
      