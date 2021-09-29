open BinFile
open ChainOp
open ListOp

type huffman_node = 
  {len : int;
    word : int;
    weight : int;
  }

let rec construct_word_layer start len word_count weight_arr =
  match word_count with
  | 0 -> ([], weight_arr)
  | n ->
    let h::t = weight_arr in
    let (next_node_list, released_weight_arr) = construct_word_layer (start + 1) len (word_count - 1) t in
    let node = {len=len; word=start; weight=h} in
    (node::next_node_list, released_weight_arr);;

let rec construct_huffman_table start len word_count_arr weight_arr = 
  match word_count_arr with
  | [] -> []
  | h::t -> 
      let (current_word_layer, released_weight_arr) = construct_word_layer len start h weight_arr in
      let next = (start + 1) * 2 in
      let next_word_layer = construct_huffman_table next (len + 1) t released_weight_arr in
      current_word_layer@next_word_layer;;

let get_huffman_table fd =
  let header = safe_read_byte fd get in
  let table_type = header / 0xf in
  let table_id = header land 0xf in
  let word_count_arr = safe_read_cnt fd 16 get in
  let word_count = total word_count_arr in
  let weight_arr = safe_read_cnt fd word_count get in
  ((construct_huffman_table 0 1 word_count_arr weight_arr, table_type, table_id), 17 + word_count);;

let rec parse_huffman_table_helper fd length =
  match length with
  | 0 -> []
  | n ->
    let (table, content_length) = get_huffman_table fd in
    table::parse_huffman_table_helper fd (length - content_length);;

let parse_huffman_table fd = 
  let length = safe_read_cnt fd 2 ByteOp.combine_and_next (minus 2) get in
  parse_huffman_table_helper fd length;;