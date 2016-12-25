(************************************************************
   http.ml		Created      : Sat Feb  8 19:40:34 2003
  			Last modified: Mon Mar 17 16:22:09 2003
  Compile: ocamlc.opt -g str.cma unix.cma url.cmo http.ml -o http #
  FTP Directory: sources/ocaml #
************************************************************)
(**

  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

*)
exception Error of string

(* 決めうち。 直すべきでしょう *)
let http_port = 80

open Url

let split_regexp = Str.regexp "\r\n\r\n"
(*let split_regexp = Str.regexp "^\r\n$"*)

let myname = Unix.gethostname ()

let get url =
  if url.protocol <> HTTP then
    raise (Error ("irregural protocol"))
  else
    let request_string =
(*      Printf.sprintf "GET %s HTTP/1.1\r\nHost: %s\r\n\r\n" url.path url.hostname in*)
      Printf.sprintf "GET %s HTTP/1.0\r\nHOST: %s\r\n\r\n" url.path myname in
    let s = Mytcp.connect url.hostname http_port in
    let buf = String.make 1 '\000' in
    let c = ref "" in
    let size =
      Unix.write s request_string 0 (String.length request_string) in
    try
      while true do
	let num = Unix.read s buf 0 1 in
	if num = 0 then
	  raise End_of_file
	else
	  c := !c^(String.sub buf 0 1)
      done;
      (* dummy *)
      ("hoge", !c)
    with
      End_of_file ->
	let content = !c in
	let size = String.length content in
	let pos = Str.search_forward split_regexp content 0 in
	let head = String.sub content 0 pos in
	let content_init_pos = Str.match_end () in
	let content = 
	  String.sub content content_init_pos (size - content_init_pos) in
	(head, content)

(*let *)
(*let get_recursive url =*)
(*  let iter u =*)
(*    let hd, tl = get_url u in*)
(*let _ = *)
(*  let res = get (Url.of_string "http://127.0.0.1/") in*)
(*  begin*)
(*    print_endline (fst res); *)
(*    print_endline "-------------------------------------------";*)
(*    print_endline (snd res)*)
(*  end*)
