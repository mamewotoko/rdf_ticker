(************************************************************
   url.ml		Created      : Sat Feb  8 19:44:23 2003
  			Last modified: 日曜日 12月 25 09:06:06 2016
   Compile: ocamlc.opt str.cma url.ml -o url #
  FTP Directory: sources/ocaml #
************************************************************)
(**

  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

*)

exception IrreguralURL of string
exception Unsupported of string

let url_regexp = 
  Str.regexp "^\\([a-z]+\\)://\\([^/]+\\)\\(.*\\)$"
(*  Str.regexp "^\\([a-z]+\\)://\\([^/]+\\)\\([^\\?]*\\)\\(?\\(.*\\)\\)?$"*)

(*let host_of url =*)
(*  Str.*)

type hostname = string
type path = string
type protocol = HTTP | FTP
type t = { protocol : protocol;
	   hostname : string;
	   path : string }

(* FTP *)
let string_of_protocol = function
    HTTP -> "http"
  | FTP -> "ftp"
  
let protocol_of_string s =
  if s = "http" then
    HTTP
  else if s = "ftp" then
    FTP
  else
    raise (Unsupported(s))

let of_string url =
  if Str.string_match url_regexp url 0 then
    let proto =
      let tmp = Str.matched_group 1 url in
      protocol_of_string tmp in
    let host = Str.matched_group 2 url in
    let path =
      let tmp = Str.matched_group 3 url in
      if tmp = "" then "/"
      else tmp in
(*    let param = *)
(*      try *)
(*	let tmp = Str.matched_group 5 url in*)
(*	Some tmp*)
(*      with *)
(*	Not_found -> None in*)
    { protocol = proto;
      hostname = host;
      path = path }
  else
    raise (IrreguralURL url)

(*let protocol_of (URL(proto, _, _, _)) = proto*)
(*let host_of (URL(_, host, _, _)) = host*)
(*let path_of (URL(_, _, path, _)) = path*)
(*let param_of (URL(_, _, _, param)) = param*)
(*let _ =*)
(*  let u = of_string test_data in*)
(*  print_endline (host_of u);*)
(*  print_endline (path_of u);*)
(*  match param_of u with*)
(*    Some(s) -> print_endline s*)
(*  | None -> print_endline "NONE"*)
