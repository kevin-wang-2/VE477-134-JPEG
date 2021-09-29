open BinFile

let rec foreach l fn = 
  match l with
  | [] -> 0
  | h :: t ->
    fn h;
    foreach t fn;;

let get arr = arr;;

let parse_app0 fd = 
  print_endline "APP0";
  safe_read_cnt fd 2 ByteOp.combine_and_next (ChainOp.minus 2) (safe_read_cnt fd) ByteOp.combine_and_next get;
  0;;

let parse_appn fd n =
  output_string stdout "APP";
  Logging.printline_int n;
  safe_read_cnt fd 2 ByteOp.combine_and_next (ChainOp.minus 2) (safe_read_cnt fd) ByteOp.combine_and_next get;
  0;;

let parse_dqt fd = 
  print_endline "DQT";
  safe_read_cnt fd 2 ByteOp.combine_and_next (ChainOp.minus 2) (safe_read_cnt fd) ByteOp.combine_and_next get;
  0;;

let parse_sof0 fd = 
  print_endline "SOF0";
  safe_read_cnt fd 2 ByteOp.combine_and_next (ChainOp.minus 2) (safe_read_cnt fd) ByteOp.combine_and_next get;
  0;;

let parse_dht fd = 
  print_endline "DHT";
  Huffman.parse_huffman_table fd;;

let parse_dri fd = 
  print_endline "DRI";
  safe_read_cnt fd 2 ByteOp.combine_and_next (ChainOp.minus 2) (safe_read_cnt fd) ByteOp.combine_and_next get;
  0;;

let parse_sos fd = 
  print_endline "SOS";
  safe_read_cnt fd 2 ByteOp.combine_and_next (ChainOp.minus 2) (safe_read_cnt fd) ByteOp.combine_and_next get;
  0;;

let parse_image_ff fd next dht = 
  let command = safe_read_cnt fd 1 ByteOp.combine_and_next get in
  match command with
  | 0xd8 -> print_endline "SOI"; 0
  | 0xe0 -> parse_app0 fd; 0
  | 0xdb -> parse_dqt fd; 0
  | 0xc0 -> parse_sof0 fd; 0
  | 0xc4 -> parse_dht fd; 0
  | 0xdd -> parse_dri fd; 0
  | 0xda -> parse_sos fd; 0
  | 0xd9 -> print_endline "EOI"; 0
  | n -> 
    if (n > 0xe0 && n <= 0xff) then 
      (parse_appn fd (n - 0xe0); 0)
    else 
      (next fd dht)

let rec parse_image fd dht = 
  let header = safe_read_cnt fd 1 ByteOp.combine_and_next get in
  match header with
  | 0xff -> parse_image_ff fd parse_image dht;
  | n -> parse_image fd dht
  ;;

let rec parse_frame fd dht =
  let header = safe_read_cnt fd 2 ByteOp.combine_and_next get in
  match header with
  | 0xffd8 -> print_endline "SOI"; parse_frame fd dht
  | 0xffe0 -> parse_app0 fd; parse_frame fd dht
  | 0xffdb -> parse_dqt fd; parse_frame fd dht
  | 0xffc0 -> parse_sof0 fd; parse_frame fd dht
  | 0xffc4 -> 
    let new_dht = dht@parse_dht fd in
    parse_frame fd new_dht
  | 0xffdd -> parse_dri fd; parse_frame fd dht
  | 0xffda -> parse_sos fd; parse_image fd dht
  | 0xffd9 -> print_endline "EOI"; 0
  | n -> 
    if (n > 0xffe0 && n <= 0xffff) then 
      (parse_appn fd (n - 0xffe0); parse_frame fd dht)
    else 
      (Printf.printf "Error: %x" n; 0)
  ;;

let read_jpeg fd next =
  parse_frame fd [];
  
  next fd;;

let () =
  safe_open_bin "./test/test.jpg" read_jpeg close_in;;
  exit 0;;

