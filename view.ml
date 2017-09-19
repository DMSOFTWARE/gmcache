(* view.ml *)

open Types
open Printf

type geo_status = {
  lat: float;
  lon: float;
  level: int;
  block: string;
  gps_lat: float;
  gps_lon: float;
  gps_hold: bool;
}

let check_cache ~cond ~create ~destroy = function
    Some pm ->
      if cond pm then pm else begin
        destroy pm;
        create ()
      end
  | None -> create ()

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

class geomap ~ues ~cache ~width ~height ?packing ?show geo =
  let da = GMisc.drawing_area ~width ~height ?packing ?show () in
  let context = da#misc#create_pango_context in
object(self)
  inherit GObj.widget_full da#as_widget

  val mutable tracks = []

  val mutable xoverlay = 1
  val mutable yoverlay = 1
    
  val mutable lat = 0.0
  val mutable lon = 0.0
  val mutable xpos = 0
  val mutable ypos = 0
  val mutable level = 1

  val mutable xcbox = 0 (* x of central block *)
  val mutable ycbox = 0 (* y of central block *)
    
  val mutable bitmap_area = (0,0,0,0) (* xa,ya,xb,yb *)
  val mutable geo_area = (0.0,0.0,0.0,0.0) (* lb,rb,tb,bb *)

  val mutable pixmap = None

  val mutable status_hooks = []

  val mutable gps_holding = false
  val mutable gps_receiver = None

  val mutable locations = []
  val mutable trackpoints = []
  val mutable checkpoints = [52.731874,41.437683; 44.679429,34.411926]

  initializer
    self # set_level 1;
    self # set_geo_coord geo;
    da#event#connect#expose
      ~callback:(fun _ -> self#draw; true);
    ()

  method set_level n =
    level <- n;
    self # set_geo_coord (lat,lon);
    da#event#connect#expose
      ~callback:(fun _ -> self#draw; true);
    ()    
      
  method set_geo_coord geo =
    check_coordinate geo;
    lat <- (fst geo);
    lon <- (snd geo);
    self # set_coord
      (bitmap_of_geo geo level);
    self # set_central ();
    self # set_bitmap_area ();
    self # call_status_hooks ()
    
  method set_coord bmp =
    xpos <- int_of_float (fst bmp);
    ypos <- int_of_float (snd bmp)
    
  method set_central () =
    xcbox <- int_of_float ((floor ((float xpos) /. (float block_size))) *. (float block_size));
    ycbox <- int_of_float ((floor ((float ypos) /. (float block_size))) *. (float block_size))

  method set_bitmap_area () =
    let (xm,ym) = ((bitmap_size level), (bitmap_size level)) in
    let xa = xcbox - (xoverlay * block_size) in
    let ya = ycbox - (yoverlay * block_size) in
    let xb = xcbox + (xoverlay * block_size) in
    let yb = ycbox + (yoverlay * block_size) in
    let (xa,xb) = if xa < 0 then 0, xb-xa else xa,xb in
    let (ya,yb) = if ya < 0 then 0, yb-ya else ya,yb in
    let (xa,xb) = if xb > xm then xa-(xb-xm),xm else xa,xb in
    let (ya,yb) = if yb > ym then ya-(yb-ym),ym else ya,yb in
    if (Config.get_param "debug") = "true" then
      printf "bitmap area: %d,%d,%d,%d\n" xa ya xb yb;

    let new_bitmap_area = (xa,ya,xb,yb) in
    let area_changed = 
      new_bitmap_area <> bitmap_area in

    bitmap_area <- new_bitmap_area;

    let geo_of_bitmap_area ba =
      let (xa,ya,xb,yb) = ba in
      let (laa,loa) = geo_of_bitmap (float xa,float ya) level in
      let (lab,lob) = geo_of_bitmap (float (xb + (xoverlay * block_size)), float (yb + (yoverlay * block_size))) level in
      let lb = min laa lab in
      let rb = max laa lab in
      let tb = max loa lob in
      let bb = min loa lob in
      (lb,rb,tb,bb)
    in
    
    if area_changed then
      begin
	geo_area <- geo_of_bitmap_area bitmap_area;	
	if level >= 16 then
	  begin
	    locations <- [];
	    List.iter
	      (fun (geo,names) ->
		locations <- (geo,names)::locations)
	      (Geonames.search ~area:geo_area);
	    flush stdout
	  end;	
	trackpoints <- Resources.search_trackpoints ~area:geo_area tracks	  
      end

  method draw_grid (w : GDraw.pixmap) =
    w # set_foreground `BLACK;
    let vlines =
      let rec make acc n =
	if n > 0 then
	  let x = n*block_size in
	  make ((x,0,x,height)::acc) (pred n)
	else
	  acc
      in make [] (xoverlay * 2)
    in
    let hlines =
      let rec make acc n =
	if n > 0 then
	  let y = n*block_size in
	  make ((0,y,width,y)::acc) (pred n)
	else
	  acc
      in make [] (yoverlay * 2)
    in
    List.iter
      (fun (xa,ya,xb,yb) ->
	w # line ~x:xa ~y:ya ~x:xb ~y:yb)
      vlines;
    List.iter
      (fun (xa,ya,xb,yb) ->
	w # line ~x:xa ~y:ya ~x:xb ~y:yb)
      hlines

  method draw_blocks ~finalize w =
    let (xo,yo,_,_) = bitmap_area in
    let real_of_local v =
      float (xo + (fst v)), 
      float (yo + (snd v)) in
    for x=0 to 2 * xoverlay do
      for y=0 to 2 * yoverlay do
	let lx = x * block_size in
	let ly = y * block_size in
	(* printf "draw block %d,%d\n" lx ly; flush stdout; *)
	let blk =
	  (block_name_with_bitmap_point
	    (real_of_local
	      (lx, ly)) level) in
	let dr =
	  new GDraw.drawable w#pixmap in
	let intermediate =
	  Filename.concat
	    (Config.get_param "share-dir") "intermediate.jpg" in
	dr#put_pixbuf ~x:lx ~y:ly
	  (GdkPixbuf.from_file intermediate);
	
	cache # get_as_file_with_callback blk
	  (function
	    | Some file ->
		dr#put_pixbuf ~x:lx ~y:ly (GdkPixbuf.from_file file);
		finalize ()
	    | None -> ())
      done
    done

  method draw_current_block (w : GDraw.pixmap) =
    let (xa,ya,xb,yb) = bitmap_area in
    let xc = xoverlay * block_size in
    let yc = yoverlay * block_size in
    if (Config.get_param "debug") = "true" then
      printf "xc: %d, yc: %d\n" xc yc;
    w # set_foreground (`NAME "red");
    w # rectangle ~x:xc ~y:yc 
      ~width:block_size
      ~height:block_size
      ~filled:false ()
      
  method draw_current_point (w : GDraw.pixmap) =
    let (xa,ya,_,_) = bitmap_area in
    w # set_foreground (`NAME "red");
    w # rectangle 
      ~x:(xpos - xa - 2) ~y:(ypos - ya - 2) 
      ~width:4 ~height:4 ()

  method draw_gps_point (w : GDraw.pixmap) =
    let (xa,ya,_,_) = bitmap_area in
    let (xpos,ypos) =
      match gps_receiver with
	| Some gps ->
	    let (a,b) = 
	      bitmap_of_geo (gps # geo ()) level in
	    int_of_float a, 
	    int_of_float b
	| None -> 4,4
    in
    w # set_foreground (`NAME "yellow");
    w # rectangle 
      ~x:(xpos - xa - 2) ~y:(ypos - ya - 2) 
      ~width:4 ~height:4 ()
    
  method draw_locations (w : GDraw.pixmap) =
    let dr =
      new GDraw.drawable w#pixmap in	
    List.iter
      (fun (geo,names) ->
	let layout = context#create_layout in
	Pango.Layout.set_text layout (List.hd (Pcre.split ~pat:"," (List.hd names)));
	let (w,h) = Pango.Layout.get_pixel_size layout in
	let (xa,ya,_,_) = bitmap_area in
	let (xg,yg) = bitmap_of_geo geo level in
	dr#put_layout 
	  ~x:(int_of_float xg - xa)
	  ~y:(int_of_float yg - ya)
	  ~fore:(`RGB (20000,20000,65000))
	  ~back:(`BLACK)
	  layout) 
      locations

  method draw_tracks (w : GDraw.pixmap) =
    let (xa,ya,_,_) = bitmap_area in
    List.iter
      (fun track ->
	let points =
	  List.map
	    (fun geo ->
	      let bmp = bitmap_of_geo geo level in
	      let x = int_of_float (fst bmp) - xa in
	      let y = int_of_float (snd bmp) - ya in
	      assert (x >= 0 && x <= width);
	      assert (y >= 0 && y <= height);
	      x, y)
	    track in
	if points <> [] then
	  begin
	    w # set_foreground (`NAME "green");
	    w # set_line_attributes ~width:2 ();
	    w # lines points;
	    w # set_line_attributes ~width:1 ();
	  end)
      trackpoints

  method draw_checkpoints (w: GDraw.pixmap) =
    let (xa,ya,_,_) = bitmap_area in
    List.iter
      (fun geo ->
	let bmp = bitmap_of_geo geo level in
	let x = int_of_float (fst bmp) - xa in
	let y = int_of_float (snd bmp) - ya in
	if x >= 0 && x <= width && y >= 0 && y <= height then
	  begin
	    w # set_foreground (`NAME "blue");
	    w # rectangle
	      ~x:(x - 4) ~y:(y - 4) 
	      ~width:8 ~height:8 ()
	  end)
      checkpoints

  method draw =
    try
      let w = check_cache pixmap
	~cond:(fun pm -> pm#size = (width, height))
	~destroy:(fun pm -> Gdk.Pixmap.destroy pm#pixmap)
	~create:
	(fun () ->
	  let size = min width height in
          context#set_font_by_name ("sans " ^ string_of_int 8);
          GDraw.pixmap ~width ~height ~window:da ())
      in
      let dr =
	new GDraw.drawable da#misc#window in
      let finalize () =
	self#draw_locations w;
	self#draw_tracks w;
	self#draw_grid w;
	self#draw_current_block w;
	self#draw_current_point w;
	self#draw_gps_point w;
	self#draw_checkpoints w;
	dr#put_pixmap ~x:0 ~y:0 w#pixmap in
      pixmap <- Some w;
      w#set_foreground `BLACK;
      w#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      self#draw_blocks ~finalize w;
      finalize ()	
    with exn ->
      print_endline (Printexc.to_string exn)

  method left () =
    let x = max 0 (xpos - block_size) in
    let (_,lon') = 
      geo_of_bitmap (float x,0.0) level in
    self # set_geo_coord (lat,lon');
    self # draw

  method right () =
    let x = min (bitmap_size level) (xpos + block_size) in
    let (_,lon') = 
      geo_of_bitmap (float x,0.0) level in
    self # set_geo_coord (lat,lon');
    self # draw

  method up () =
    let y = max 0 (ypos - block_size) in
    let (lat',_) = 
      geo_of_bitmap (0.0,float y) level in
    self # set_geo_coord (lat',lon);
    self # draw

  method down () =
    let y = min (bitmap_size level) (ypos + block_size) in
    let (lat',_) = 
      geo_of_bitmap (0.0,float y) level in
    self # set_geo_coord (lat',lon);
    self # draw

  method zoom_in () =
    print_endline "zoom in";
    level <- (min 20 (succ level));
    self # set_geo_coord (lat,lon);
    self # draw

  method zoom_out () =
    print_endline "zoom out";
    level <- (max 1 (pred level));
    self # set_geo_coord (lat,lon);
    self # draw

  method hold () =
    gps_holding <- not gps_holding;
    (match gps_receiver with
      | Some r ->
	  r # active gps_holding
      | None -> ());
    self # call_status_hooks ()

  method trace_track () : unit =
    let level_stream = Stream.of_list [14] in
    let point_stream = Stream.of_list (List.flatten tracks) in
    
    let rec next_level f =
      try
	let level =
	  Stream.next level_stream in
	self # set_level level;
	next_point level 0 f
      with Stream.Failure ->
	cache # loader # drop_queue_empty_hook ()
    and next_point level counter f =
      try
	let processed = f (Stream.next point_stream) counter in
	if processed then
	  cache # loader # set_queue_empty_hook
	    (fun () ->
	      next_point level (succ counter) f)
	else
	  next_point level (succ counter) f
      with Stream.Failure ->
	next_level f
    in
    
    next_level
      (fun point counter ->
	if counter mod (int_of_float (1000.0 /. (float_of_int level))) = 0 then
	  begin
	    self # set_geo_coord point;
	    self # draw;
	    true
	  end
	else
	  false)

  method set_gps_receiver (r : Gps.receiver) =
    gps_receiver <- Some r;
    r # add_geo_hook 
      (fun geo ->	
	self # set_geo_coord geo;
	self#draw)

  method status () =
    let (glat,glon) =
      match gps_receiver with
	| Some gps ->
	    gps # geo ()
	| None -> 0.0,0.0
    in
    {
      lat = lat;
      lon = lon;
      level = level;
      block = block_name (lat,lon) level;
      gps_lat = glat;
      gps_lon = glon;
      gps_hold = gps_holding;
    }

  method add_status_hook f =
    status_hooks <- f::status_hooks

  method call_status_hooks () =
    List.iter 
      (fun f ->
	f (self#status ()))
      status_hooks

  method set_tracks file =
    tracks <- Resources.load_tracks ~hd:0 ~tl:0 file;
    self # draw;

  method drop_tracks () =
    tracks <- [];
    self # draw

end

let message s () =  
  GToolbox.message_box "Message" s

let dynamic_message f () =
  GToolbox.message_box "Message" (f ())

let main ues ~lat ~lon ~level (geocache : Cache.block) =
  print_endline "=> gmcache viewer";

  let locale = GtkMain.Main.init () in
  
  let w = GWindow.window
    ~title:"gmcache viewer"
    ~resizable:true
    ~position:`CENTER
    ~border_width:2
    ~allow_grow:true
    ~allow_shrink:true
    ~width:(block_size * 4)
    ~height:(block_size * 3)
    ()
  in

  let hbox = 
    GPack.hbox ~border_width:0 ~packing:w#add () in

  let g = new geomap
    ~ues
    ~cache:geocache
    ~width:(block_size * 3)
    ~height:(block_size * 3)
    ~packing:hbox#add 
    ~show:true
    (lat,lon) in

  let vbox =
    GPack.vbox ~packing:hbox#add () in
 
  let fix = GPack.fixed 
    ~packing:vbox#add
    ~width:block_size
    ~height:(block_size / 2 - 20)
    () in

  let bbox =
    GPack.button_box `VERTICAL
      ~height:(block_size * 2)
      ~child_width:240
      ~border_width:0
      ~layout:`START
      ~packing:vbox#add () in

  GMisc.separator `HORIZONTAL ~packing:vbox#add ();
 
  let lt =
    GButton.button
      ~label:"Load tracks" ~packing:bbox#add () in

  let dt =
    GButton.button
      ~label:"Drop tracks" ~packing:bbox#add () in

  let status =
    GText.view
      ~width:block_size
      ~height:(block_size / 2 - 20)
      ~editable:false () in

  let view_tracks_selector () =
    let w = GWindow.window
      ~title:"track selector"
      ~resizable:false
      ~position:`CENTER
      ~border_width:2
      ~allow_grow:false
      ~allow_shrink:false
      ~width:(block_size * 2)
      ~height:(block_size)
      ()
    in
    let vbox = 
      GPack.vbox ~border_width:0 ~packing:w#add () in
    let c = GEdit.combo 
      ~popdown_strings:(Resources.track_list ())
      ~allow_empty:true
      ~enable_arrow_keys:true
      ~packing:vbox#add
      ()
    in
    let b = GButton.button ~label:"OK" ~packing:vbox#add () in
    b # connect # clicked 
      ~callback:(fun () -> 
	let file = 
	  Filename.concat 
	    (Config.get_param "resource-dir") ("tracks/" ^ c#entry#text)
	in g#set_tracks file;
	w#destroy ());
    w # show ()
  in

  lt # connect # clicked ~callback:view_tracks_selector;
  dt # connect # clicked ~callback:g#drop_tracks;

  g # add_status_hook
    (fun s ->
      let b = Buffer.create 32 in
      let a = Buffer.add_string b in
      a (sprintf "block..... %s\n" s.block);
      a (sprintf "lat....... %f\n" s.lat);
      a (sprintf "lon....... %f\n" s.lon);
      a (sprintf "level..... %d\n" s.level);
      a (sprintf "gps-lat... %f\n" s.gps_lat);
      a (sprintf "gps-lon... %f\n" s.gps_lon);
      a (sprintf "gps-hold.. %b\n" s.gps_hold);
      status # buffer # set_text (Buffer.contents b));
  fix # put status#coerce ~x:0 ~y:0;
  g # call_status_hooks ();
  g # set_level level;

  (* set gps-receiver *)
  let gps = 
    new Gps.receiver ues in
  g # set_gps_receiver gps;

  let quit () =
    gps # abort (); 
    GMain.Main.quit ()
  in

  (* set callbacks *)
  let usage = Buffer.create 32 in
  let usadd = Buffer.add_string usage in
  let callbacks =
    [
      GdkKeysyms._k, [`MOD1], "Alt-k", "Hello", message "Hello";
      GdkKeysyms._w, [], "w", "World", message "World";
      GdkKeysyms._q, [`CONTROL], "Ctrl-q", "Quit", quit;
      GdkKeysyms._h, [`CONTROL], "Ctrl-h", "Help", dynamic_message (fun () -> (Buffer.contents usage));
      GdkKeysyms._Left,  [], "Left",  "Left",  g#left;
      GdkKeysyms._Right, [], "Right", "Right", g#right;
      GdkKeysyms._Up,    [], "Up",    "Up",    g#up;
      GdkKeysyms._Down,  [], "Down",  "Down",  g#down;
      GdkKeysyms._Up,    [`SHIFT], "Shift+Up",  "Zoom in",  g#zoom_in;
      GdkKeysyms._Down,  [`SHIFT], "Shift+Down","Zoom out", g#zoom_out;
      GdkKeysyms._g, [`CONTROL], "Ctrl-g", "GPS hold (on/off)", g#hold;
      GdkKeysyms._t, [`CONTROL], "Ctrl-t", "Trace track", g#trace_track;
    ]
  in

  usadd "Available shortcuts:\n";
  List.iter
    (fun (k,mods,s,l,f) ->
      Okey.add w ~mods k f;
      usadd (sprintf "%s: %s\n" s l))
    callbacks;

  w # connect # destroy ~callback:quit;
  w # show ();
  GMain.main ()



