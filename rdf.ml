(************************************************************
   rdf.ml		Created      : Sat Mar 15 13:33:39 2003
  			Last modified: Sun Dec 25 08:12:31 2016
   Compile: ocamlc expat.cma unix.cma utf.ml rdf.ml -o rdf #
  FTP Directory: sources/ocaml #
************************************************************)
(**

  @author Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

*)

(*let inputfile filename =*)
(*  let i = open_in filename in*)
(*  let rec iter result =*)
(*    try*)
(*      iter (result^(input_line i)^"\n");*)
(*    with End_of_file -> result in*)
(*  let result = iter "" in*)
(*  begin close_in i; result end*)

let get_titles rdf_content = 
  let is_item_mode = ref false in
  let do_get_content = ref false in
  let titles = ref [] in

  let start_element_handler tag attr_list =
    if tag = "item" then
      is_item_mode := true
    else if tag = "title" then
      if !is_item_mode then
	do_get_content := true
      else ()
    else () in
  
  let end_element_handler tag =
    if tag = "item" then
      is_item_mode := false
    else if tag = "title" then
      do_get_content := false
    else () in
  
  let character_data_hander x =
    if !do_get_content then
      titles := (Utf.utf8_to_euc x)::!titles
    else () in

  let p = Expat.parser_create (Some "UTF-8") in
  Expat.set_start_element_handler p start_element_handler;
  Expat.set_end_element_handler p end_element_handler;
  Expat.set_character_data_handler p character_data_hander;
  (try
    Expat.parse p rdf_content;
    Expat.final p;
  with Expat.Expat_error e ->
    begin prerr_endline (Expat.xml_error_to_string e); exit 1 end);
  List.rev !titles
