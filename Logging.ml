open Printf
open String

let secure_open filename fn = 
  let fd = open_out filename in
    try
      fn fd
    with e ->
      close_out_noerr fd;
      raise e;;

let to_hex_char i = 
  match i with
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"
  | 10 -> "a"
  | 11 -> "b"
  | 12 -> "c"
  | 13 -> "d"
  | 14 -> "e"
  | n -> "f"

let rec string_of_hex i=
  if i < 16 then to_hex_char i else ((string_of_hex (i / 16) ^ to_hex_char (i mod 16)))

let fprint_int fd i = output_string fd (string_of_int i);;
let fprint_hex fd i = output_string fd (string_of_hex i);;
let fprint_char_of_int fd i = output_char fd (char_of_int i);;
let fnewline fd = output_char fd '\n';;

let fprintline_hex fd i = fprint_hex fd i;fnewline fd;;
let fprintline_int fd i = fprint_int fd i;fnewline fd;;

let print_int i = fprint_int stdout i;;
let print_hex i = fprint_hex stdout i;;
let print_char_of_int i = fprint_char_of_int stdout i;;
let newline () = fnewline stdout;;

let printline_hex i = print_hex i;newline ();;
let printline_int i = print_int i;newline ();;