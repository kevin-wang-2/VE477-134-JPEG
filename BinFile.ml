let safe_open_bin filename fn = 
  let fd = open_in_bin filename in 
    try
      fn fd
    with e->
      close_in_noerr fd;
      raise e;;

let rec read_cnt_helper fd cnt arr fn =
  match cnt with
  | 0 ->
    fn arr
  | n -> read_cnt_helper fd (cnt - 1) (arr @ [(input_byte fd)]) fn;;

let read_cnt fd cnt fn = 
  read_cnt_helper fd cnt [] fn;;
  
let rec safe_read_cnt_helper fd cnt arr fn=
  try
    match cnt with
    | 0 -> fn arr
    | n -> safe_read_cnt_helper fd (cnt - 1) (arr @ [(input_byte fd)]) fn
  with e ->
    Logging.secure_open "test.log" output_string "EOF";
    close_in_noerr fd;
    fn arr;;

let safe_read_cnt fd cnt fn = 
  safe_read_cnt_helper fd cnt [] fn;;

let safe_read_byte fd fn =
  try
    fn (input_byte fd)
  with e ->
    Logging.secure_open "test.log" output_string "EOF";
    close_in_noerr fd;
    raise e;
      