(* fetch.ml *)

open Printf
open Nethttp_client

exception URL_fetching_problem of string

let string_of_status = function
  | `Unserved -> "Unserved"
  | `Http_protocol_error exn ->
      sprintf "Http_protocol_errro(%s)"
	(Printexc.to_string exn)
  | `Successful -> "Successful"
  | `Redirection -> "Redirection"
  | `Client_error -> "Client_error"
  | `Server_error -> "Server_error"

let url_of_blockname n =
  let node = Random.int 3 in  
  sprintf "http://khm%d.google.com/kh?n=404&hl=en&v=%s&t=%s"
    node (Config.get_param "maps-version") n

let make_kh_cookies
  ~khcookie ~pref ~nid () =
  let p k v = k ^ "=" ^ v in
  String.concat "; "
    [
      (p "khcookie" khcookie);
      (p "PREF" pref);
      (p "NID" nid);
      (*p "S" s*)
    ]

let kh_cookies =
  make_kh_cookies
    ~khcookie:(Config.get_param "cookie-khcookie")
    ~pref:(Config.get_param "cookie-pref")
    ~nid:(Config.get_param "cookie-nid")
    (* ~s:"sorry=d273niiW2rpRXVKRrIaH2Q" *)
    ()

let delay ues timeout f =
  let g  = ues # new_group () in
  let id = ues # new_wait_id () in
  let op = Unixqueue.Wait id in
  let handler ues ev = function
    | Unixqueue.Timeout(g',op')
	when g' = g && op' = op -> 
	ues # clear g; f ()
    | _ -> raise Equeue.Reject
  in
    ues # add_resource g (op, timeout);
    ues # add_handler g handler

let create_loop ues = 
  ues # new_group ()

let set_loop ues g f =
  let timeout = 1.0 in
  let id = ues # new_wait_id () in
  let op = Unixqueue.Wait id in
  let handler ues ev = function
    | Unixqueue.Timeout(g',op')
	when g' = g && op' = op -> f ()
    | _ -> raise Equeue.Reject
  in ues # add_resource g (op, timeout);
  ues # add_handler g handler

let drop_loop ues g =
  ues # clear g

class downloader ues =
object(self)

  val pipe = new pipeline
    
  val mutable queue_empty_hook = (fun () -> print_endline "default queue empty hook")
  val mutable hook_loop = None

  val mutable tasks = 0

  initializer
    pipe # set_event_system ues;
    if bool_of_string (Config.get_param "debug") then
      pipe # set_options
	{ pipe # get_options with 
	  verbose_status = true;
	  verbose_connection = true;
	  verbose_request_header = true;
	  verbose_request_contents = true;
	  verbose_response_header = true;
	  verbose_response_contents = true;
	};
    pipe # set_proxy_from_environment ~insecure:true ()

  method call url =
    let call = new get url in
    let rh = call # request_header `Base in
    rh # update_field "User-Agent" (Config.get_param "user-agent");
    rh # update_field "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8";
    rh # update_field "Accept-Language" "ru,en-us;q=0.7,en;q=0.3";
    rh # update_field "Accept-Encoding" "gzip,deflate";
    rh # update_field "Accept-Charset"  "windows-1251,utf-8;q=0.7,*;q=0.7";
    rh # update_field "Referer"         "http://maps.google.com/";
    rh # update_field "Cookie"          kh_cookies;
    call

  method check_hooks () =
    if tasks = 0 then
      begin
	print_endline "loader # tasks is done";
	queue_empty_hook ();
      end
    else
      print_endline 
	(sprintf "loader # wait tasks: %d" tasks)

  method analyze url call =    
    match call # status with
      | `Successful ->
	  if call # response_status_code = 200 then
	    Some call # response_body # value
	  else
	    begin
	      printf "WRONG HTTP STATUS %d\n\n%s\n"
		call # response_status_code
		call # response_body # value;
	      None
	    end
      | status ->
	  print_endline
	    (sprintf "%s while load [%s]" (string_of_status status) url);
	  None

  method link url callback =
    let call = self # call url in
    let callback_wrapper call =
      tasks <- pred tasks;
      callback (self # analyze url call) in
    tasks <- succ tasks;
    pipe # add_with_callback
      call callback_wrapper
	  
  method block name callback =
    if bool_of_string (Config.get_param "offline") then
      callback None
    else
      begin
	delay ues (float_of_string (Config.get_param "fetch-delay"))
	  (fun () ->
	    print_endline (" load> " ^ name);
	    self # link (url_of_blockname name) callback)
      end
	
  method set_queue_empty_hook f =
    let loop = create_loop ues in
    hook_loop <- Some loop;
    set_loop ues loop 
      self#check_hooks;
    queue_empty_hook <- f

  method drop_queue_empty_hook () =
    (match hook_loop with
	Some loop ->
	  drop_loop ues loop;
	  hook_loop <- None
      | None -> ());
    queue_empty_hook <- (fun _ -> ())
	
end





