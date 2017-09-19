(* gps.ml *)

open Types
open Printf

exception Rfcomm_error
exception Error of string

let string_of_channel ch =
  let b = Buffer.create 32 in
  let rec read () =
    try
      read (Buffer.add_string b (input_line ch))
    with End_of_file -> Buffer.contents b
  in read ()

let read_lines ?(env=Unix.environment()) ?(ignore_error=false) ?(filter=(fun _ -> true)) command =
  let (ch,out,err) = Unix.open_process_full command env in
  let rec read acc =
    try
      let s = input_line ch in
      if filter s then
	read (acc @ [s])
      else
	read acc
    with End_of_file ->
      if not ignore_error then
	let error = string_of_channel err in
	match Unix.close_process_full (ch,out,err) with
	  | Unix.WEXITED 0 -> acc
	  | _ ->
	      let curenv =
		let b = Buffer.create 32 in
		Array.iter
		  (fun s ->
		    Buffer.add_string b s;
		    Buffer.add_char b '\n')
		  env;
		Buffer.contents b
	      in
	      raise
		(Error
		  (sprintf "cannot read lines from [%s] (%s) with env:\n\n%s" command error curenv))
      else acc
  in read []

let rsplit ?(strict=true) d s =
  let l = String.length s in
  let b = Buffer.create 32 in
  let rec make acc n =
    if l > n then
      let next = succ n in
      let state = s.[n] = d in
      if state then
	let prev_state = 
	  n <> 0 && s.[pred n] = d in
	if prev_state <> state || strict then
	  let m = 
	    Buffer.contents b 
	  in Buffer.clear b;
	  make (m::acc) next
	else
	  make acc next
      else
	(Buffer.add_char b s.[n]; make acc next)
    else       
      if Buffer.length b > 0 then
	(Buffer.contents b)::acc
      else acc
  in make [] 0

let split ?(strict=true) d s =
  List.rev (rsplit ~strict d s)

class base_reader netbuffer ues =
object(self)
  inherit [unit] Uq_engines.engine_mixin (`Working 0) ues
  val outbuffer = new Netchannels.output_netbuffer netbuffer

  (* Implementation of the async_out_channel
     without output method *)
  method flush () = ()
  method pos_out  = outbuffer # pos_out
  method close_out = outbuffer # close_out
  method can_output = true
end

type geohook = coordinate -> unit

class receiver ues =
  let netbuffer = Netbuffer.create 1024 in
object(self)
  inherit base_reader netbuffer ues

  val mutable local_lat = 0.0
  val mutable local_lon = 0.0
  val mutable curr_lat = 0.0
  val mutable curr_lon = 0.0
  val mutable geo_hook = ((fun _ -> ()) : geohook)
  val mutable holding = false
  val mutable eng = None

  initializer
    self # prepare ()

  val nmea_messages = Queue.create ()

  method geo () = curr_lat,curr_lon

  method active v = 
    self # log_message 
      (sprintf "gps # active %b" v);
    holding <- v;
    if holding && (local_lat <> 0.0 || local_lon <> 0.0) then
      geo_hook (local_lat,local_lon)

  method add_geo_hook f = geo_hook <- f

  method set_link () =
    print_endline "set gps-link";
    let gps_required =
      Config.get_param "gps-required" <> "false" in
    let serial_device =
      Config.get_param "gps-serial" in
    try
      let fd = Unix.openfile serial_device [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o640 in
      let r =
	new Uq_transfer.receiver
	  ~src:fd
	  ~dst:self
	  ~close_src:true
	  ~close_dst:true
	ues in
      eng <- Some r;
      let verbose =
	bool_of_string (Config.get_param "verbose") in
      Uq_engines.when_state
	~is_done:(fun _ ->
	  if verbose then
	    self # log_message "gps receiver is done";
	  if gps_required then
	    self # prepare ())
	~is_aborted:(fun () ->
	  if verbose then 
	    self # log_message "gps receiver is aborted";
	  self # rfcomm 
	    ~face:(Config.get_param "gps-btface") 
	    "release")
	~is_error:(fun exn ->
	  if verbose then
	    self # log_message 
	      (sprintf "gps receiver is error (%s)"
		(Printexc.to_string exn));
	  if gps_required then
	    self # prepare ())
	r
    with exn ->
      print_endline (sprintf "Warning: gps-receiver deactivated by errors (%s)" (Printexc.to_string exn));
      if gps_required then
	begin
	  if Sys.file_exists serial_device then
	    begin
	      print_endline "waiting...";
	      Unix.sleep 3;
	      self # set_link ()
	    end
	  else
	    self # prepare ()
	end

  method rfcomm ?callback ?face ?addr ?chan action =
    let cmd = Buffer.create 16 in
    let add = Buffer.add_string cmd in
    add "rfcomm "; add action;
    (match face with Some s -> add " "; add s | None -> ());
    (match addr with Some s -> add " "; add s | None -> ());
    (match chan with Some s -> add " "; add s | None -> ());
    
    let command = Buffer.contents cmd in
    
    print_endline
      (sprintf "run: %s" command);
    flush stdout;
    
    match callback with
	None ->
	  let rc = Sys.command command in
	  if rc <> 0 then
	    begin
	      print_endline
		(sprintf "Warning: problem while run [%s]" command);
	      raise Rfcomm_error
	    end
      | Some cback ->
	  let child () =
	    let value = function
	      | Some s -> s
	      | None -> assert false
	    in Unix.execvp "rfcomm"
		 [|"rfcomm";action;(value face);(value addr)|]
	  in
	  let parent pid =
	    let eng =
	      new Uq_engines.poll_process_engine ~period:1.0 ~pid ues in
	    Uq_engines.when_state
	      ~is_done:(fun process_status ->
		print_endline "rfcomm disconnection";
		cback ())
	      ~is_error:(fun exn ->
		print_endline (sprintf "rfcomm disconnection by error (%s)" (Printexc.to_string exn));
		cback ())
	      eng
	  in
	  let pid = Unix.fork () in
	  if pid > 0 then
	    parent pid
	  else 
	    child ()
	  
  method prepare () =
    print_endline "gps # prepare";
    let gps_required =
      Config.get_param "gps-required" <> "false" in
    if gps_required then
      try
	let addr =
	  Config.get_param "gps-btaddr" in
	let face =
	  Config.get_param "gps-btface" in
	while
	  self # scanning addr
	do Unix.sleep 3 done;      
	print_endline "device found";
	if Sys.file_exists (Config.get_param "gps-serial") then
	  self # rfcomm ~face "release";
	let callback () =
	  (* called when action is finished *)
	  self # prepare () 
	in
	self # rfcomm ~callback ~face ~addr "connect";
	print_endline "waiting 5 seconds..."; Unix.sleep 5;
	self # set_link ()
      with Rfcomm_error -> 
	print_endline "waiting 3 seconds..."; Unix.sleep 3;
	if gps_required then
	  self # prepare ()

  method scanning addr =
    print_endline
      (sprintf "gps # scanning on %s" addr);
    let devices =
      List.map 
	(fun s ->
	  let dev = 
	    List.nth (Pcre.split ~pat:"\\s+" s) 1 in
	  printf " - %s\n" dev; flush stdout; dev)
	(List.tl (read_lines "hcitool inq --flush"))
    in not (List.mem addr devices)
    
  method abort () =
    (match eng with
	Some e ->
	  e # abort ()
      | None -> ())

  method log_message s =
    print_endline s
    
  method update geo =
    curr_lat <- fst geo;
    curr_lon <- snd geo;
    if holding then
      if local_lat <> (fst geo) || local_lon <> (snd geo) then
	begin
	  local_lat <- (fst geo);
	  local_lon <- (snd geo);
	  geo_hook geo;
	end
      else ()
    else
      begin
	local_lat <- 0.0;
	local_lon <- 0.0;
      end
      
  method nmea_handler s =
    let prep_coord ~inv n =
      (*
       * NMEA format for:
       * -  Latitude: ddmm.mmmm
       * -  Longitude: dddmm.mmmm
       *)
      let l = String.length n in
      let m =
	String.sub n (l - 7) 7 in
      let d =
	String.sub n 0 (l - 7) in
      let r =
	(float_of_int (int_of_string d)) +. ((float_of_string m) /. 60.0) in
      if inv then (-. r) else r
    in

    (* todo: verify NMEA messages by checksum *)

    let len = String.length s in
    if len > 6 then
      let msg_type =
	String.sub s 1 5 in
      match msg_type with
	| "GPRMC" ->
	    begin
	      let rmc = split ~strict:false ',' s in
	      if List.length rmc > 10 then
		begin
		  let utc = List.nth rmc 1 in
		  let vdr  = List.nth rmc 4 in
		  let hdr  = List.nth rmc 6 in
		  let lat = prep_coord ~inv:(vdr <> "N") (List.nth rmc 3) in
		  let lon = prep_coord ~inv:(hdr <> "E") (List.nth rmc 5) in
		  self # update (lat,lon)
		end
	    end
	| _ -> ()
	      
  method output s k n =
    let m = outbuffer # output s k n in
    let local_buffer = Buffer.create 256 in
    let counter = ref 0 in
    let real_counter = ref 0 in
    String.iter
      (fun c ->
	incr counter;
	match c with
	  | '\r' -> ()
	  | '\n' ->
	      self # nmea_handler (Buffer.contents local_buffer);
	      Buffer.reset local_buffer;
	      real_counter := !counter;
	  |  c   -> Buffer.add_char local_buffer c)
      (Netbuffer.contents netbuffer);
    Netbuffer.delete netbuffer 0 !real_counter;
    m
    
end
  
  
