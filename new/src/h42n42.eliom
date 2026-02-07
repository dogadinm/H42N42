(* h42n42.eliom — реализация проекта H42N42 на Eliom *)

let%server application_name = "h42n42"
let%client application_name = Eliom_client.get_application_name ()

module%shared App = Eliom_registration.App (struct
  let application_name = application_name
  let global_data_path = Some ["__global_data__"]
end)

let%client _ = Eliom_client.persist_document_head ()

(* -------- СЕРВИС ГЛАВНОЙ СТРАНИЦЫ -------- *)

let%server main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%client main_service = ~%main_service

(* -------- КЛИЕНТСКАЯ ЧАСТЬ -------- *)

[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

module Html = Dom_html
let document = Html.document

module D = Eliom_content.Html.D

(* --- параметры игры (можешь балансить, но смысл не меняем) --- *)

let initial_creets = 10

let base_size = 24.0
let base_speed = 60.0

let infection_slow_factor = 0.85          (* Sick moves 15% slower :contentReference[oaicite:8]{index=8} *)
let infection_prob_per_tick = 0.02        (* 2% per contact/iteration :contentReference[oaicite:9]{index=9} *)

let berserk_prob = 0.10                   (* check once every 10s after contamination :contentReference[oaicite:10]{index=10} *)
let mean_prob    = 0.10                   (* check once every 10s after contamination :contentReference[oaicite:11]{index=11} *)

let evo_check_interval = 10.0             (* every 10 seconds after contamination :contentReference[oaicite:12]{index=12} *)
let berserk_growth_interval = 10.0        (* +10% diameter every 10 seconds :contentReference[oaicite:13]{index=13} *)
let berserk_growth_factor = 1.10
let berserk_max_factor = 4.0              (* dies at 4× :contentReference[oaicite:14]{index=14} *)

let mean_size_factor = 0.85               (* 15% smaller :contentReference[oaicite:15]{index=15} *)
let mean_ttl = 60.0                       (* dies after exactly 1 minute :contentReference[oaicite:16]{index=16} *)

let dir_change_prob = 0.02                (* "not too frequently" *)
let reproduction_interval = 5.0           (* spawn periodically while at least one healthy :contentReference[oaicite:17]{index=17} *)
let panic_speed_increase_per_sec = 0.03   (* difficulty progression speed increase :contentReference[oaicite:18]{index=18} *)

let river_height = 80.0
let hospital_height = 80.0

(* --- состояние крипов --- *)

type state =
  | Healthy
  | Sick
  | Berserk
  | Mean

type creep = {
  mutable x       : float;
  mutable y       : float;
  mutable vx      : float;
  mutable vy      : float;
  mutable size    : float;        (* diameter *)
  base_size       : float;
  mutable state   : state;

  (* timers *)
  mutable infected_age : float;   (* time since became Sick/Berserk/Mean *)
  mutable next_evo_check : float; (* next time (since infection) when we do 10s evolution check *)
  mutable berserk_acc : float;    (* accum time for 10s growth steps *)
  mutable mean_age : float;       (* time since became Mean *)

  elt             : Html.divElement Js.t;

  mutable grabbed : bool;
  mutable dragged_by_player : bool; (* true if currently in a user drag session *)
  mutable alive   : bool;
}

(* глобальные ссылки *)

let creeps : creep list ref = ref []
let game_div_ref : Html.divElement Js.t option ref = ref None
let game_width  : float ref = ref 800.0
let game_height : float ref = ref 500.0
let elapsed_time : float ref = ref 0.0
let game_over = ref false

(* --- утилиты --- *)

let rand_range a b = a +. Random.float (b -. a)
let clamp a x b = if x < a then a else if x > b then b else x

let speed_factor () =
  let t = !elapsed_time in
  clamp 1.0 (1.0 +. panic_speed_increase_per_sec *. t) 3.0

let desired_speed (st: state) =
  let base = base_speed *. speed_factor () in
  match st with
  | Healthy -> base
  | Sick | Berserk | Mean -> base *. infection_slow_factor  (* PDF explicitly says Sick slower; ok to keep same slow for special types *)

let set_state_color (c : creep) =
  let style = c.elt##.style in
  let color =
    match c.state with
    | Healthy -> "#44dd44"   (* green *)
    | Sick    -> "#ff5555"   (* red *)
    | Berserk -> "#ff00ff"   (* magenta/purple *)
    | Mean    -> "#ffaa00"   (* orange *)
  in
  style##.backgroundColor := Js.string color

let update_dom (c : creep) =
  let s = c.elt##.style in
  s##.left   := Js.string (Printf.sprintf "%gpx" c.x);
  s##.top    := Js.string (Printf.sprintf "%gpx" c.y);
  s##.width  := Js.string (Printf.sprintf "%gpx" c.size);
  s##.height := Js.string (Printf.sprintf "%gpx" c.size)

let radius (c : creep) = c.size /. 2.0

let center (c : creep) =
  (c.x +. radius c, c.y +. radius c)

let dist2 (c1 : creep) (c2 : creep) =
  let x1,y1 = center c1 in
  let x2,y2 = center c2 in
  let dx = x1 -. x2 in
  let dy = y1 -. y2 in
  dx *. dx +. dy *. dy

let is_contaminated_state = function
  | Healthy -> false
  | Sick | Berserk | Mean -> true

let is_healthy c = c.alive && (c.state = Healthy)

let any_healthy () =
  List.exists (fun c -> is_healthy c) !creeps

let healthy_count () =
  List.fold_left (fun acc c -> if is_healthy c then acc + 1 else acc) 0 !creeps

(* --- убийство крипа --- *)

let kill_creep (c : creep) =
  if not c.alive then ()
  else begin
    c.alive <- false;
    c.grabbed <- false;
    c.dragged_by_player <- false;
    Js.Opt.iter c.elt##.parentNode (fun p ->
      Dom.removeChild p c.elt
    );
    creeps := List.filter (fun x -> x != c) !creeps
  end

(* --- заражение / лечение / эволюция --- *)

let random_velocity () =
  let angle = rand_range 0.0 (2.0 *. Float.pi) in
  let speed = desired_speed Healthy in
  (speed *. cos angle, speed *. sin angle)

let renormalize_velocity (c : creep) =
  let cur = sqrt (c.vx *. c.vx +. c.vy *. c.vy) in
  let target = desired_speed c.state in
  if cur <= 0.000001 then begin
    (* если вдруг ноль — зададим случайно *)
    let vx, vy = random_velocity () in
    c.vx <- vx; c.vy <- vy
  end else begin
    let k = target /. cur in
    c.vx <- c.vx *. k;
    c.vy <- c.vy *. k
  end

let become_sick (c : creep) =
  if (not c.alive) || c.state <> Healthy then ()
  else begin
    c.state <- Sick;
    c.infected_age <- 0.0;
    c.next_evo_check <- evo_check_interval;
    c.berserk_acc <- 0.0;
    c.mean_age <- 0.0;
    c.size <- c.base_size;
    renormalize_velocity c;
    set_state_color c;
    update_dom c
  end

let become_berserk (c : creep) =
  c.state <- Berserk;
  c.size <- c.base_size;       (* starts at base, then grows stepwise *)
  c.berserk_acc <- 0.0;
  (* infected_age continues; but evolution checks stop because state != Sick *)
  renormalize_velocity c;
  set_state_color c;
  update_dom c

let become_mean (c : creep) =
  c.state <- Mean;
  c.size <- c.base_size *. mean_size_factor;  (* 85% :contentReference[oaicite:19]{index=19} *)
  c.mean_age <- 0.0;
  renormalize_velocity c;
  set_state_color c;
  update_dom c

let heal (c : creep) =
  (* Только обычный Sick лечится; Berserk/Mean нельзя лечить :contentReference[oaicite:20]{index=20} *)
  if not c.alive then ()
  else if c.state <> Sick then ()
  else begin
    c.state <- Healthy;
    c.infected_age <- 0.0;
    c.next_evo_check <- evo_check_interval;
    c.berserk_acc <- 0.0;
    c.mean_age <- 0.0;
    c.size <- c.base_size;
    renormalize_velocity c;
    set_state_color c;
    update_dom c
  end

(* --- зоны --- *)

let is_in_river (c : creep) =
  (* touch toxic river at top -> sick immediately :contentReference[oaicite:21]{index=21} *)
  c.y <= river_height -. c.size *. 0.3

let is_in_hospital (c : creep) =
  (* hospital at bottom :contentReference[oaicite:22]{index=22} *)
  c.y +. c.size >= !game_height -. hospital_height +. c.size *. 0.3

(* --- создание крипов (TyXML для динамических элементов) --- *)

let create_dom_creep (game_div : Html.divElement Js.t) :
  Html.divElement Js.t * float * float =
  let node =
    D.div
      ~a:[
        D.a_style "position:absolute;border-radius:50%;cursor:pointer;box-shadow:0 0 6px rgba(0,0,0,0.6);"
      ]
      []
  in
  (* ВАЖНО: используем To_dom из Eliom, а не Tyxml_js *)
  let d = Eliom_content.Html.To_dom.of_div node in

  let margin = 30.0 in
  let x = rand_range margin (!game_width -. margin -. base_size) in
  let y = rand_range (river_height +. margin)
            (!game_height -. hospital_height -. margin -. base_size) in
  Dom.appendChild game_div d;
  (d, x, y)

let make_creep (game_div : Html.divElement Js.t) : creep =
  let elt, x, y = create_dom_creep game_div in
  let vx, vy = random_velocity () in
  let c = {
    x; y; vx; vy;
    size = base_size;
    base_size;
    state = Healthy;

    infected_age = 0.0;
    next_evo_check = evo_check_interval;
    berserk_acc = 0.0;
    mean_age = 0.0;

    elt;
    grabbed = false;
    dragged_by_player = false;
    alive = true;
  } in
  set_state_color c;
  update_dom c;
  c

(* --- динамика движения --- *)

let maybe_random_change_dir (c : creep) =
  if Random.float 1.0 < dir_change_prob then begin
    let angle_delta = rand_range (-0.6) 0.6 in
    let angle = atan2 c.vy c.vx +. angle_delta in
    let speed = desired_speed c.state in
    c.vx <- speed *. cos angle;
    c.vy <- speed *. sin angle;
  end

let bounce_from_walls (c : creep) =
  (* bounce at edges (angle incidence = reflection) :contentReference[oaicite:24]{index=24} *)
  let maxx = !game_width -. c.size in
  let maxy = !game_height -. c.size in

  if c.x < 0.0 then begin
    c.x <- 0.0;
    c.vx <- -. c.vx;
  end else if c.x > maxx then begin
    c.x <- maxx;
    c.vx <- -. c.vx;
  end;

  if c.y < 0.0 then begin
    c.y <- 0.0;
    c.vy <- -. c.vy;
  end else if c.y > maxy then begin
    c.y <- maxy;
    c.vy <- -. c.vy;
  end

let handle_river_contamination (c : creep) =
  (* Invulnerable while dragging :contentReference[oaicite:25]{index=25} *)
  if c.grabbed || not c.alive then () else
  if c.state = Healthy && is_in_river c then become_sick c

let handle_contact_infection (src : creep) =
  if (not src.alive) then ()
  else if (not (is_contaminated_state src.state)) || src.grabbed then ()
  else
    List.iter
      (fun dst ->
         (* healthy + not being dragged => can be infected :contentReference[oaicite:26]{index=26} *)
         if dst.alive && dst.state = Healthy && (not dst.grabbed) then
           let rsum = radius src +. radius dst in
           if dist2 src dst <= rsum *. rsum then
             if Random.float 1.0 < infection_prob_per_tick then
               become_sick dst)
      !creeps

let chase_nearest_healthy (c : creep) =
  (* Mean actively chases nearest healthy :contentReference[oaicite:27]{index=27} *)
  if (not c.alive) || c.state <> Mean then () else
  let healthy_list =
    List.filter (fun h -> is_healthy h && (not h.grabbed)) !creeps
  in
  match healthy_list with
  | [] -> ()
  | _ ->
    let cx, cy = center c in
    let best =
      List.fold_left
        (fun acc h ->
           let hx, hy = center h in
           let dx = hx -. cx in
           let dy = hy -. cy in
           let d2 = dx *. dx +. dy *. dy in
           match acc with
           | None -> Some (d2, dx, dy)
           | Some (best_d2, _, _) ->
             if d2 < best_d2 then Some (d2, dx, dy) else acc)
        None healthy_list
    in
    match best with
    | None -> ()
    | Some (_, dx, dy) ->
      let norm = sqrt (dx *. dx +. dy *. dy) in
      if norm <= 0.000001 then () else
      let dirx = dx /. norm in
      let diry = dy /. norm in
      let sp = desired_speed Mean in
      c.vx <- sp *. dirx;
      c.vy <- sp *. diry

(* --- эволюция Sick -> Berserk/Mean (check once every 10 seconds) --- *)

let maybe_evolve (c : creep) =
  (* Only while state = Sick; cannot become both :contentReference[oaicite:28]{index=28} *)
  if c.state <> Sick then ()
  else if c.infected_age +. 1e-9 < c.next_evo_check then ()
  else begin
    (* one check per 10s mark; if stays Sick, schedule next *)
    let r = Random.float 1.0 in
    if r < berserk_prob then become_berserk c
    else if r < berserk_prob +. mean_prob then become_mean c
    else c.next_evo_check <- c.next_evo_check +. evo_check_interval
  end

let update_berserk_growth (c : creep) (dt : float) =
  if c.state <> Berserk then () else begin
    (* +10% every 10 seconds :contentReference[oaicite:29]{index=29} *)
    c.berserk_acc <- c.berserk_acc +. dt;
    while c.berserk_acc +. 1e-9 >= berserk_growth_interval do
      c.berserk_acc <- c.berserk_acc -. berserk_growth_interval;
      c.size <- c.size *. berserk_growth_factor;
    done;
    (* death at 4× original diameter :contentReference[oaicite:30]{index=30} *)
    if c.size +. 1e-9 >= c.base_size *. berserk_max_factor then
      kill_creep c
    else
      update_dom c
  end

let update_mean_death (c : creep) (dt : float) =
  if c.state <> Mean then () else begin
    c.mean_age <- c.mean_age +. dt;
    if c.mean_age +. 1e-9 >= mean_ttl then
      kill_creep c
  end

(* --- перетаскивание мышью --- *)

let init_drag (game_div : Html.divElement Js.t) (c : creep) =
  let open Lwt_js_events in

  (* Berserk/Mean cannot be grabbed/healed :contentReference[oaicite:31]{index=31} *)
  let can_grab () =
    c.alive && (c.state = Healthy || c.state = Sick)
  in

  let get_local_coords (ev : Html.mouseEvent Js.t) =
    (* clientX/Y -> coords relative to game_div *)
    let rect = game_div##getBoundingClientRect in
    let mx = float_of_int ev##.clientX in
    let my = float_of_int ev##.clientY in
    let left = rect##.left in
    let top  = rect##.top in
    (mx -. left, my -. top)
  in

  ignore
    (mousedowns c.elt (fun ev _ ->
         if not (can_grab ()) then Lwt.return_unit else
         let ev = (ev :> Html.mouseEvent Js.t) in

         c.grabbed <- true;
         c.dragged_by_player <- true;

         let cx0, cy0 = get_local_coords ev in
         let start_x = c.x in
         let start_y = c.y in

         let rec drag_loop () =
           Lwt.pick [
             (mousemove document >>= fun ev_move ->
                if (not c.alive) then Lwt.return_unit else
                let evm = (ev_move :> Html.mouseEvent Js.t) in
                let cx, cy = get_local_coords evm in
                let dx = cx -. cx0 in
                let dy = cy -. cy0 in

                (* drop anywhere INCLUDING outside boundaries -> NO clamp :contentReference[oaicite:32]{index=32} *)
                c.x <- start_x +. dx;
                c.y <- start_y +. dy;

                update_dom c;
                drag_loop ()
             );

             (mouseup document >>= fun _ ->
                c.grabbed <- false;

                (* heal only if manually dropped in hospital, and ONLY Sick :contentReference[oaicite:33]{index=33} *)
                if c.alive && c.dragged_by_player && is_in_hospital c then
                  heal c;

                c.dragged_by_player <- false;
                Lwt.return_unit
             )
           ]
         in
         Lwt.async drag_loop;
         Lwt.return_unit
      ))

(* --- глобальный поток времени --- *)

let rec time_thread () =
  if !game_over then Lwt.return_unit
  else
    Lwt_js.sleep (1.0 /. 60.0) >>= fun () ->
    elapsed_time := !elapsed_time +. (1.0 /. 60.0);
    time_thread ()

(* --- поток для каждого крипа --- *)

let rec creep_thread (game_div : Html.divElement Js.t) (c : creep) =
  if !game_over || not c.alive then Lwt.return_unit
  else
    Lwt_js.sleep (1.0 /. 60.0) >>= fun () ->
    if !game_over || not c.alive then Lwt.return_unit
    else begin
      let dt = 1.0 /. 60.0 in

      (* timers only for contaminated kinds *)
      if is_contaminated_state c.state then c.infected_age <- c.infected_age +. dt;

      (* evolve check: once every 10s after contamination while Sick :contentReference[oaicite:34]{index=34} *)
      maybe_evolve c;

      (* mean death: exactly 1 minute being mean :contentReference[oaicite:35]{index=35} *)
      update_mean_death c dt;

      (* berserk growth & death by size :contentReference[oaicite:36]{index=36} *)
      if c.alive then update_berserk_growth c dt;

      if (not c.alive) then Lwt.return_unit
      else begin
        if not c.grabbed then begin
          (* keep speed consistent with difficulty progression *)
          renormalize_velocity c;

          (* behavior *)
          maybe_random_change_dir c;
          chase_nearest_healthy c;

          (* move *)
          c.x <- c.x +. c.vx *. dt;
          c.y <- c.y +. c.vy *. dt;

          (* boundaries *)
          bounce_from_walls c;

          (* river contamination *)
          handle_river_contamination c;

          update_dom c;
        end;

        (* contact infection (no collisions, only contamination) :contentReference[oaicite:37]{index=37} *)
        handle_contact_infection c;

        creep_thread game_div c
      end
    end

(* --- репродукция --- *)

let add_creep_from_reproduction () =
  match !game_div_ref with
  | None -> ()
  | Some game_div ->
    let c = make_creep game_div in
    creeps := c :: !creeps;
    init_drag game_div c;
    Lwt.async (fun () -> creep_thread game_div c)

let rec reproduction_thread () =
  if !game_over then Lwt.return_unit
  else
    Lwt_js.sleep reproduction_interval >>= fun () ->
    if !game_over then Lwt.return_unit
    else begin
      if any_healthy () then add_creep_from_reproduction ();
      reproduction_thread ()
    end

(* --- очистка / рестарт / game over overlay --- *)

let clear_game () =
  game_over := true;

  List.iter (fun c ->
    c.alive <- false;
    c.grabbed <- false;
    c.dragged_by_player <- false;
    Js.Opt.iter c.elt##.parentNode (fun p ->
      Dom.removeChild p c.elt
    )
  ) !creeps;

  (try
     let ov = Dom_html.getElementById "game-over-overlay" in
     Js.Opt.iter ov##.parentNode (fun p -> Dom.removeChild p ov)
   with _ -> ());

  creeps := [];
  elapsed_time := 0.0

let rec show_game_over_overlay () =
  match !game_div_ref with
  | None -> ()
  | Some game_div ->
    let overlay = Html.createDiv document in
    overlay##.id := Js.string "game-over-overlay";
    let s = overlay##.style in
    s##.position := Js.string "absolute";
    s##.left := Js.string "0";
    s##.top := Js.string "0";
    s##.width := Js.string "100%";
    s##.height := Js.string "100%";
    s##.backgroundColor := Js.string "rgba(0,0,0,0.65)";
    s##.display := Js.string "flex";
    ignore (s##setProperty (Js.string "justify-content") (Js.string "center") Js.Optdef.empty);
    ignore (s##setProperty (Js.string "align-items") (Js.string "center") Js.Optdef.empty);

    let box = Html.createDiv document in
    let bs = box##.style in
    bs##.display := Js.string "flex";
    ignore (bs##setProperty (Js.string "flex-direction") (Js.string "column") Js.Optdef.empty);
    ignore (bs##setProperty (Js.string "gap") (Js.string "18px") Js.Optdef.empty);
    ignore (bs##setProperty (Js.string "align-items") (Js.string "center") Js.Optdef.empty);

    let title = Html.createDiv document in
    let ts = title##.style in
    ts##.fontSize := Js.string "42px";
    ts##.color := Js.string "#ffffff";
    ts##.fontFamily := Js.string "sans-serif";
    title##.textContent := Js.some (Js.string "GAME OVER");

    let btn = Html.createButton document in
    let bts = btn##.style in
    bts##.fontSize := Js.string "20px";
    bts##.padding := Js.string "10px 18px";
    bts##.border := Js.string "0";
    bts##.borderRadius := Js.string "10px";
    bts##.cursor := Js.string "pointer";
    btn##.textContent := Js.some (Js.string "Restart");

    let restart () =
      clear_game ();
      game_over := false;
      (* стартуем заново *)
      (match !game_div_ref with
       | None -> ()
       | Some gd ->
         game_width := float_of_int gd##.offsetWidth;
         game_height := float_of_int gd##.offsetHeight;

         let created = List.init initial_creets (fun _ -> make_creep gd) in
         creeps := created;
         List.iter (fun c ->
           init_drag gd c;
           Lwt.async (fun () -> creep_thread gd c)
         ) created;

         Lwt.async reproduction_thread;
         Lwt.async time_thread;
         Lwt.async game_over_thread;
      )
    in

    btn##.onclick := Html.handler (fun _ ->
      restart ();
      Js._false
    );

    Dom.appendChild box title;
    Dom.appendChild box btn;
    Dom.appendChild overlay box;
    Dom.appendChild game_div overlay

and game_over_thread () =
  if !game_over then Lwt.return_unit
  else
    Lwt_js.sleep 0.3 >>= fun () ->
    if !game_over then Lwt.return_unit
    else begin
      (* game ends when no healthy remains :contentReference[oaicite:38]{index=38} *)
      if healthy_count () = 0 then begin
        game_over := true;
        show_game_over_overlay ();
        Lwt.return_unit
      end else
        game_over_thread ()
    end

(* --- старт --- *)

let start_game () =
  Random.self_init ();

  let game_div =
    match Html.getElementById "game"
          |> Html.CoerceTo.div
          |> Js.Opt.to_option with
    | None -> failwith "No #game div"
    | Some d -> d
  in
  game_div_ref := Some game_div;

  game_width := float_of_int game_div##.offsetWidth;
  game_height := float_of_int game_div##.offsetHeight;

  let created = List.init initial_creets (fun _ -> make_creep game_div) in
  creeps := created;

  List.iter (fun c ->
    init_drag game_div c;
    Lwt.async (fun () -> creep_thread game_div c)
  ) created;

  Lwt.async reproduction_thread;
  Lwt.async time_thread;
  Lwt.async game_over_thread

let () =
  Eliom_client.onload (fun () ->
    game_over := false;
    start_game ();
    ()
  )
]

(* -------- РЕГИСТРАЦИЯ СЕРВИСА И РАЗМЕТКА -------- *)

let%shared () =
  App.register ~service:main_service (fun () () ->
    Lwt.return
      Eliom_content.Html.F.(
        html
          (head
             (title (txt "H42N42"))
             [ css_link
                 ~uri:
                   (make_uri
                      ~service:(Eliom_service.static_dir ())
                      ["css"; "h42n42.css"])
                 () ])
          (body [
             h1
               ~a:[a_style "text-align:center; font-family:sans-serif; color:#eee;"]
               [txt "H42N42"];

             div
               ~a:[a_id "game-container";
                   a_style
                     "position:relative;\
                      width:1100px;\
                      height:700px;\
                      margin:30px auto;\
                      background:#111;\
                      border:2px solid #555;\
                      box-shadow:0 0 15px rgba(0,0,0,0.6);\
                      overflow:hidden;"]
               [
                 div
                   ~a:[a_id "game";
                       a_style
                         "position:absolute;\
                          left:0; top:0;\
                          width:100%; height:100%;\
                          overflow:hidden;"]
                   [
                     div
                       ~a:[a_id "river";
                           a_style
                             "position:absolute;\
                              left:0; top:0;\
                              width:100%; height:80px;\
                              background:#2e8bff;\
                              opacity:0.5;\
                              pointer-events:none;"]
                       [txt "Toxic River"];

                     div
                       ~a:[a_id "hospital";
                           a_style
                             "position:absolute;\
                              left:0; bottom:0;\
                              width:100%; height:80px;\
                              background:#4caf50;\
                              opacity:0.5;\
                              pointer-events:none;"]
                       [txt "Hospital"];
                   ];
               ];
          ])))
