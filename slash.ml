(************************************************************
   slash.ml		Created      : Sat Mar 15 15:55:10 2003
  			Last modified: 日曜日 12月 25 09:15:16 2016
   Compile: ocamlc -g -thread -I +labltk labltk.cma expat.cma utf.ml rdf.ml str.cma /usr/local/lib/ocaml/unix.cma threads.cma mytcp.ml url.ml http.ml slash.ml -o slash #
  FTP Directory: sources/ocaml #
  gtar cfvz slash.tar.gz utf.ml rdf.ml mytcp.ml url.ml http.ml slash.ml
************************************************************)
(**

  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

*)

open Tk

let appname = "Headlines  "
let urls = [
  ((Url.of_string "http://bulknews.net/rss/rdf.cgi?Asahi"), "朝日 [http://www.asahi.com]");
  ((Url.of_string "http://slashdot.jp/slashdot.rdf"), "/. [http://slashdot.jp]"); 
]

let get_titles () = 
  List.fold_right (fun (url, descr) l -> 
    let (head, content) = Http.get url in
    ((List.map (fun x -> (x, descr)) (Rdf.get_titles content)) @ l)) urls []

let titles = ref (Array.of_list (get_titles ()))
let is_new = ref false
let sleep_time = 60.0 *. 35.0 (* sec *)
let display_sleep_time = 15.0 

let get_thread_function () =
  let rec iter () =
print_endline "get titles";
    titles := Array.of_list (get_titles ());
    is_new := true;
    Thread.delay sleep_time;
    iter () in
  iter ()

let display_thread_function (label,window) =
print_endline "display titles";
  let cancel_function i label =
    i := true in
  let rec iter i size content =
    if !is_new then
      let new_size = Array.length content in
      begin 
	is_new := false;
	iter 0 new_size !titles
      end
    else
      begin
	let (title, descr) = content.(i) in
	Label.configure ~text:title label;
	Wm.title_set window (appname^descr);
	Thread.delay display_sleep_time;
	iter ((i+1) mod size) size content
      end in
  iter 0 (Array.length !titles) !titles
    
let _ = 
  let _ = print_endline "start" in
  let window = openTk () in
(*  let font = "-ricoh-gothic-medium-r-*-*-14-*" in*)
(*  let font = "-ricoh-gothic-medium-r-*-*-14-*" in*)
  let font = "-misc-fixed-medium-r-*-*-14-120-*" in
  let _ = print_endline "get font" in
  let label = Label.create ~font ~background:`Black ~foreground:`White
      ~width:80 window in
(*  Array.iter print_endline !titles;*)
(*  flush stdout;*)
    print_endline "get appname";
  appname_set appname;
  ignore (Thread.create get_thread_function ());
  ignore (Thread.create display_thread_function (label,window));
  pack [label];
  mainLoop ()
