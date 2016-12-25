(************************************************************
   utf.ml		Created      : Fri Mar 14 23:15:14 2003
  			Last modified: Sat Mar 15 14:39:08 2003
  Compile: ocamlc -g utf.ml -o utf #
  FTP Directory: sources/ocaml #
************************************************************)
(**
   不完全です。
  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
  http://www.unicode.org/Public/MAPPINGS/OBSOLETE/EASTASIA/JIS/JIS0208.TXT
*)

let table_file_name = "JIS0208.TXT"

let sjis_code_start = 0x8140 
let sjis_code_end = 0xEAA4 

let additional = 0x8080
let euc_code_start = 0x2121 + additional
let euc_code_end = 0x7426 + additional

let read_table_file s e input =
  let table = Array.make (e-s+1) 0 in
  let inv_table = Array.make 0xFFFF 0 in
  let rec iter () =
    let line = input_line input in
    if line.[0] = '#' then
      iter ()
    else
      begin
	Scanf.sscanf line "%i\t%i\t%i"
	  (fun _ code unicode ->
	    table.(code+additional-s) <- unicode;
	    inv_table.(unicode) <- code+additional);
	iter ()
      end in
  try
    iter ()
  with End_of_file ->
    (table, inv_table)

let (euc2utf_table, utf2euc_table) = 
  let input = open_in table_file_name in
  let (table, inv_table) = read_table_file euc_code_start euc_code_end input in
  begin close_in input; (table, inv_table) end

let euc_to_utf8 code =
  let end_pos = String.length code in
  let buffer = Buffer.create (end_pos * 3) in
  let rec iter pos =
    if pos >= end_pos then
      ()
    else
      let c1code = code.[pos] in
      let c1 = Char.code c1code in
      if c1 <= 0x7F then
	begin
	  Buffer.add_char buffer c1code;
(*	  buffer.[write_pos] <- c1code;*)
	  iter (pos+1) 
	end
      else
	let code = 
	  let c2 = Char.code code.[pos+1] in
	  let index = (c1 lsl 8) lor c2 in
	  euc2utf_table.(index - euc_code_start) in
	if code <= 0x7FF then
	  let cl = (code land 0x3F) lor 0x80 in (* 6 bit *)
	  let ch = ((code lsr 6) land 0x1F) lor 0xC0 in
	  begin
	    Buffer.add_char buffer (Char.chr ch);
	    Buffer.add_char buffer (Char.chr cl);
(*	    buffer.[write_pos] <- Char.chr ch;*)
(*	    buffer.[write_pos+1] <- Char.chr cl;*)
	    iter (pos+2)
	  end
	else if code <= 0xFFFF then
	  begin
	    Buffer.add_char buffer (Char.chr (((code lsr 12) land 0xF) lor 0xE0));
	    Buffer.add_char buffer (Char.chr (((code lsr 6) land 0x3F) lor 0x80));
	    Buffer.add_char buffer (Char.chr ((code land 0x3F) lor 0x80)); (* 6 bit *)
(*	    buffer.[write_pos] <- (Char.chr (((code lsr 12) land 0xF) lor 0xE0));*)
(*	    buffer.[write_pos+1] <- (Char.chr ((code land 0x3F) lor 0x80)); (* 6 bit *)*)
(*	    buffer.[write_pos+2] <- (Char.chr (((code lsr 6) land 0x3F) lor 0x80));*)
	    iter (pos+2)
	  end
	else 
	  raise Not_found in
  begin 
    iter 0;
    Buffer.contents buffer
  end
    
let utf8_to_euc code =
  let end_pos = String.length code in
  let buffer = Buffer.create ((end_pos * 2) / 3) in
  let rec iter pos = 
    if pos >= end_pos then
      ()
    else
      let c1 = code.[pos] in
      let c1code = Char.code c1 in
      if c1code land 0x80 = 0 then
	begin
	  Buffer.add_char buffer c1;
	  iter (pos+1)
	end
      else 
	let c2code = Char.code code.[pos+1] in
	let (wc, next_pos) =
	  if c1code land 0xE0 = 0xC0 then
	    (((c1code land 0x1F) lsl 6) lor (c2code land 0x3F), pos+2)
	  else if c1code land 0xF0 = 0xE0 then
	    let c3code = Char.code code.[pos+2] in
	    (((c1code land 0xF) lsl 12)
	      lor ((c2code land 0x3F) lsl 6)
	       lor (c3code land 0x3F), pos+3)
	  else
	    begin 
	      Printf.printf "%08X\n" (c1code land 0xE0);
	      raise Not_found
	    end in
(*	let _ = Printf.printf "Searching wc = %04X\n" wc in*)
	let rc = utf2euc_table.(wc) in
	let rc1 = rc lsr 8 in
	let rc2 = rc land 0xFF in
	begin
	  Buffer.add_char buffer (Char.chr rc1);
	  Buffer.add_char buffer (Char.chr rc2);
	  iter next_pos
	end in
  begin iter 0; Buffer.contents buffer end

let rec encode_from_input input =
  let line = input_line input in
  try
    print_endline (euc_to_utf8 line);
    encode_from_input input
  with End_of_file -> ()

let rec decode_from_input input =
  let line = input_line input in
  try
    print_endline (utf8_to_euc line);
    decode_from_input input
  with End_of_file -> ()
 
