(************************************************************
   mytcp.ml		Created      : Sun Mar  2 19:38:17 2003
  			Last modified: Sun Mar 02 19:46:09 2003
  Compile: ocamlopt.opt mytcp.ml -o mytcp #
  FTP Directory: sources/ocaml #
************************************************************)
(**

  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

*)

module Unix = UnixLabels

exception Message of string

let connect host port =
  let addr = Unix.gethostbyname host in
  let s = 
    Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_STREAM ~protocol:0 in
  Unix.connect s (Unix.ADDR_INET(addr.Unix.h_addr_list.(0), port)); s
;;

(* ある特定の文字列パターンがきたら終り *)
let receive_until_pattern socket pattern = 
  let pattern_size = String.length pattern in
  let c = ref "" in
  let buf = String.make 1 '\000' in
  try
    while true do
      let num = Unix.read socket buf 0 1 in
      if num = 0 then
	if !c = "" then
	  raise End_of_file
	else
	  raise (Message(!c))
      else
	let ch = String.sub buf 0 num in
	begin
	  c := !c^ch;
	  let size = String.length !c in
	  if size >= pattern_size then
	    let last = Str.last_chars !c pattern_size in
	    if last = pattern then
	      raise (Message(!c))
	end
    done;
    "hoge"
  with
    Message c -> c
;;
	  
let receive_one_line socket =
  (receive_until_pattern socket) "\n"
;;
  
let send_string socket m =
  let size = String.length m in
  Unix.send socket m 0 size []
;;    

