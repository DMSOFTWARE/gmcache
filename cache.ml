(* cache.ml *)

open Printf

exception Error of string
exception Invalid_content of string
exception Cannot_create_directory of string

let make_directory dir =
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755
  else if Sys.is_directory dir then
    ()
  else
    raise (Cannot_create_directory dir)

let readfile file =
  let b = Buffer.create 32 in
  let a = Buffer.add_char b in
  let ch = open_in_bin file in
  try
    while true do
      a (input_char ch)
    done; assert false
  with End_of_file ->
    close_in ch;
    Buffer.contents b
  
class block ues storage =
  let loader = new Fetch.downloader ues in
object(self)

  method verify content =
    let io =
      Unix.open_process "file -i -" in
    output_string (snd io) content;
    flush (snd io); close_out (snd io);
    let s = input_line (fst io) in
    (match Unix.close_process io with
      | Unix.WEXITED 0 -> 
	  let rex = 
	    Pcre.regexp "jpeg" in
	  Pcre.pmatch ~rex s
      | _ -> 
	  print_endline "problem with content verification";
	  false)
      
  method store name content =
    let name = 
      if self # verify content
      then name else "problem"
    in make_directory storage;
    let file =
      Filename.concat storage name in
    let ch = open_out file in
    output_string ch content;
    close_out ch
  
  method content file =
    let buf = Buffer.create 8192 in
    let ch = open_in_bin file in
    try
      while true do
	Buffer.add_char
	  buf (input_char ch)
      done; assert false
    with End_of_file ->
      Buffer.contents buf

  method exists name =
    Sys.file_exists
      (Filename.concat storage name)

  method file name =
    Filename.concat storage name

  method problem =
    Filename.concat 
      (Config.get_param "share-dir") "problem.jpg"

  method get_as_file_with_callback name callback =
    if self # exists name then
      callback (Some (self # file name))
    else
      begin
	let callback_wrapper content =
	  match content with
	    | Some s ->
		self # store name s;
		if self # exists name then
		  callback (Some (self # file name))
		else
		  callback (Some self # problem)
	    | None ->
		callback None in
	loader # block name callback_wrapper
      end

  method loader = loader

end

