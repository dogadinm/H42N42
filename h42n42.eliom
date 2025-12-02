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

(* -------- КЛИЕНТСКАЯ ЧАСТЬ: КРИПЫ, ИГРА, LWT-ПОТОКИ -------- *)

[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Infix

module Html = Dom_html
let document = Html.document

(* --- параметры игры --- *)

let initial_creets = 12
let base_size = 24.0
let base_speed = 90.0              (* базовая скорость здоровых *)
let infection_slow_factor = 0.85   (* -15% скорости при заражении *)
let berserk_grow_factor_max = 4.0  (* берсерк вырастает до 4x *)
let mean_size_factor = 0.85        (* злой уменьшается до 0.85 размера *)
let dir_change_prob = 0.02         (* шанс сменить направление на тик *)
let infection_prob_per_tick = 0.02 (* 2% за итерацию при контакте *)
let berserk_prob = 0.10
let mean_prob = 0.10
let reproduction_interval = 5.0    (* каждые N секунд, пока есть здоровые *)
let panic_speed_increase_per_sec = 0.03  (* глобальное ускорение с течением времени *)

(* размеры зон (внутри div#game) *)
let river_height = 80.0      (* верхняя токсичная зона *)
let hospital_height = 80.0   (* нижняя зона больницы *)

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
  mutable size    : float;   (* текущий диаметр *)
  base_size       : float;   (* базовый диаметр *)
  mutable state   : state;
  mutable infection_age : float;  (* сколько времени заражён *)
  elt             : Html.divElement Js.t;
  mutable grabbed : bool;
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
  (* со временем растёт до 3x макс *)
  clamp 1.0 (1.0 +. panic_speed_increase_per_sec *. t) 3.0

let set_state_color (c : creep) =
  let style = c.elt##.style in
  let color =
    match c.state with
    | Healthy -> "#44dd44"   (* зелёный *)
    | Sick    -> "#ff5555"   (* красный *)
    | Berserk -> "#ff00ff"   (* фиолетовый *)
    | Mean    -> "#ffaa00"   (* жёлто-оранжевый *)
  in
  style##.backgroundColor := Js.string color

let update_dom (c : creep) =
  let s = c.elt##.style in
  s##.left   := Js.string (Printf.sprintf "%gpx" c.x);
  s##.top    := Js.string (Printf.sprintf "%gpx" c.y);
  s##.width  := Js.string (Printf.sprintf "%gpx" c.size);
  s##.height := Js.string (Printf.sprintf "%gpx" c.size)

let radius (c : creep) = c.size /. 2.0

(* центр круга *)
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

let is_healthy c = (c.state = Healthy)

let any_healthy () =
  List.exists (fun c -> is_healthy c) !creeps

let healthy_count () =
  List.fold_left (fun acc c -> if is_healthy c then acc + 1 else acc) 0 !creeps

(* --- заражение, лечение, изменение состояния --- *)

let recalc_velocity_with_panic (c : creep) =
  let current_speed = sqrt (c.vx *. c.vx +. c.vy *. c.vy) in
  if current_speed = 0.0 then () else
  let factor =
    match c.state with
    | Healthy -> speed_factor ()
    | Sick -> speed_factor () *. infection_slow_factor
    | Berserk -> speed_factor () *. 1.3
    | Mean -> speed_factor () *. 1.2
  in
  let new_speed = base_speed *. factor in
  let k = new_speed /. current_speed in
  c.vx <- c.vx *. k;
  c.vy <- c.vy *. k

let infect (c : creep) =
  if not (is_healthy c) then ()
  else begin
    c.infection_age <- 0.0;
    (* медленнее *)
    c.vx <- c.vx *. infection_slow_factor;
    c.vy <- c.vy *. infection_slow_factor;
    (* выбор: берсерк / mean / просто sick *)
    let r = Random.float 1.0 in
    if r < berserk_prob then begin
      c.state <- Berserk;
      c.size <- c.base_size;
    end else if r < berserk_prob +. mean_prob then begin
      c.state <- Mean;
      c.size <- c.base_size *. mean_size_factor;
    end else begin
      c.state <- Sick;
      c.size <- c.base_size;
    end;
    set_state_color c;
    update_dom c;
  end

let heal (c : creep) =
  c.state <- Healthy;
  c.infection_age <- 0.0;
  c.size <- c.base_size;
  (* вернуть нормальную скорость в том же направлении *)
  let speed = base_speed *. speed_factor () in
  let norm = sqrt (c.vx *. c.vx +. c.vy *. c.vy) in
  let dirx, diry =
    if norm = 0.0 then (1.0, 0.0)
    else (c.vx /. norm, c.vy /. norm)
  in
  c.vx <- speed *. dirx;
  c.vy <- speed *. diry;
  set_state_color c;
  update_dom c

(* --- зоны --- *)

let is_in_river (c : creep) =
  (* касание верхнего токсичного слоя *)
  c.y <= river_height -. c.size *. 0.3

let is_in_hospital (c : creep) =
  (* касание нижней больницы *)
  c.y +. c.size >= !game_height -. hospital_height +. c.size *. 0.3

(* --- создание крипов --- *)

let create_dom_creep (game_div : Html.divElement Js.t) :
  Html.divElement Js.t * float * float =
  let d = Html.createDiv document in
  let s = d##.style in
  s##.position := Js.string "absolute";
  s##.borderRadius := Js.string "50%";
  ignore (s##setProperty
            (Js.string "box-shadow")
            (Js.string "0 0 6px rgba(0,0,0,0.6)")
            Js.Optdef.empty);
  s##.cursor := Js.string "pointer";
  (* стартовая позиция — не слишком близко к реке/больнице *)
  let margin = 30.0 in
  let x = rand_range margin (!game_width -. margin -. base_size) in
  let y = rand_range (river_height +. margin)
            (!game_height -. hospital_height -. margin -. base_size) in
  Dom.appendChild game_div d;
  (d, x, y)

let random_velocity () =
  let angle = rand_range 0.0 (2.0 *. Float.pi) in
  let speed = base_speed in
  (speed *. cos angle, speed *. sin angle)

let make_creep (game_div : Html.divElement Js.t) : creep =
  let elt, x, y = create_dom_creep game_div in
  let vx, vy = random_velocity () in
  let c = {
    x; y; vx; vy;
    size = base_size;
    base_size;
    state = Healthy;
    infection_age = 0.0;
    elt;
    grabbed = false;
  } in
  set_state_color c;
  update_dom c;
  c

(* --- динамика --- *)

let update_berserk_growth (c : creep) dt =
  match c.state with
  | Berserk ->
    c.infection_age <- c.infection_age +. dt;
    (* пусть за 20 секунд растёт до 4x *)
    let t = c.infection_age in
    let factor =
      1.0 +. (berserk_grow_factor_max -. 1.0) *. (t /. 20.0)
    in
    let factor = clamp 1.0 factor berserk_grow_factor_max in
    c.size <- c.base_size *. factor;
    update_dom c
  | _ -> ()

let chase_nearest_healthy (c : creep) =
  if c.state <> Mean then () else
  let healthy_list =
    List.filter (fun h -> is_healthy h && (not h.grabbed)) !creeps
  in
  match healthy_list with
  | [] -> () (* никого нет, пусть летит по старой траектории *)
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
           | None -> Some (h, d2, dx, dy)
           | Some (_, best_d2, _, _) ->
             if d2 < best_d2 then Some (h, d2, dx, dy) else acc)
        None healthy_list
    in
    match best with
    | None -> ()
    | Some (_, _, dx, dy) ->
      let norm = sqrt (dx *. dx +. dy *. dy) in
      if norm = 0.0 then () else
      let dirx = dx /. norm in
      let diry = dy /. norm in
      let speed = base_speed *. speed_factor () *. 1.5 in
      c.vx <- speed *. dirx;
      c.vy <- speed *. diry

let maybe_random_change_dir (c : creep) =
  if Random.float 1.0 < dir_change_prob then begin
    let angle_delta = rand_range (-0.6) 0.6 in
    let angle = atan2 c.vy c.vx +. angle_delta in
    let speed = sqrt (c.vx *. c.vx +. c.vy *. c.vy) in
    c.vx <- speed *. cos angle;
    c.vy <- speed *. sin angle;
  end

let bounce_from_walls (c : creep) =
  let maxx = !game_width -. c.size in
  let maxy = !game_height -. c.size in
  if c.x < 0.0 then begin
    c.x <- 0.0;
    c.vx <- -. c.vx;
  end;
  if c.x > maxx then begin
    c.x <- maxx;
    c.vx <- -. c.vx;
  end;
  if c.y < 0.0 then begin
    c.y <- 0.0;
    c.vy <- -. c.vy;
  end;
  if c.y > maxy then begin
    c.y <- maxy;
    c.vy <- -. c.vy;
  end

let handle_river_contamination (c : creep) =
  if c.grabbed then () else
  if is_healthy c && is_in_river c then infect c

let handle_contact_infection (src : creep) =
  if (not (is_contaminated_state src.state)) || src.grabbed then ()
  else
    List.iter
      (fun dst ->
         if is_healthy dst && (not dst.grabbed) then
           let rsum = radius src +. radius dst in
           if dist2 src dst <= rsum *. rsum then
             if Random.float 1.0 < infection_prob_per_tick then
               infect dst)
      !creeps

(* --- перетаскивание мышью через Lwt_js_event --- *)

let init_drag (c : creep) =
  let open Lwt_js_events in
  let _ =
    mousedowns c.elt (fun ev _ ->
        let ev = (ev :> Html.mouseEvent Js.t) in
        c.grabbed <- true;
        let start_x = c.x in
        let start_y = c.y in
        let mx0 = float_of_int ev##.clientX in
        let my0 = float_of_int ev##.clientY in

        let rec drag_loop () =
          Lwt.pick [
            (mousemove document >>= fun ev_move ->
             let evm = (ev_move :> Html.mouseEvent Js.t) in
             let mx = float_of_int evm##.clientX in
             let my = float_of_int evm##.clientY in
             let dx = mx -. mx0 in
             let dy = my -. my0 in
             c.x <- clamp 0.0 (start_x +. dx) (!game_width -. c.size);
             c.y <- clamp 0.0 (start_y +. dy) (!game_height -. c.size);
             update_dom c;
             drag_loop ()
            );
            (mouseup document >>= fun ev_up ->
             let _ = (ev_up :> Html.mouseEvent Js.t) in
             c.grabbed <- false;
             (* при отпускании в больнице лечим заражённых *)
             if is_in_hospital c && is_contaminated_state c.state then heal c;
             Lwt.return_unit)
          ]
        in
        Lwt.async drag_loop;
        Lwt.return_unit
      )
  in
  ()

(* --- поток для каждого крипа --- *)

let rec creep_thread (c : creep) =
  if !game_over then Lwt.return_unit
  else
    let%lwt () = Lwt_js.sleep (1.0 /. 60.0) in
    if !game_over then Lwt.return_unit
    else begin
      let dt = 1.0 /. 60.0 in
      elapsed_time := !elapsed_time +. dt;

      if not c.grabbed then begin
        (* паника — ускорение *)
        recalc_velocity_with_panic c;
        (* случайное изменение направления *)
        maybe_random_change_dir c;
        (* злые преследуют здоровых *)
        chase_nearest_healthy c;
        (* шаг движения *)
        c.x <- c.x +. c.vx *. dt;
        c.y <- c.y +. c.vy *. dt;
        (* отскоки *)
        bounce_from_walls c;
        (* река заражает *)
        handle_river_contamination c;
        (* рост берсерка *)
        update_berserk_growth c dt;
        (* обновление DOM *)
        update_dom c;
      end;

      (* заражаем других при контакте *)
      handle_contact_infection c;

      creep_thread c
    end

(* --- создание нового крипа из репродукции --- *)

let add_creep_from_reproduction () =
  match !game_div_ref with
  | None -> ()
  | Some game_div ->
    let c = make_creep game_div in
    creeps := c :: !creeps;
    init_drag c;
    Lwt.async (fun () -> creep_thread c)

(* --- GAME OVER оверлей --- *)

let show_game_over_overlay () =
  match !game_div_ref with
  | None -> ()
  | Some game_div ->
    let overlay = Html.createDiv document in
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
    s##.fontSize := Js.string "42px";
    s##.color := Js.string "#ffffff";
    s##.fontFamily := Js.string "sans-serif";
    overlay##.innerHTML := Js.string "GAME OVER";
    Dom.appendChild game_div overlay

(* --- мониторинг конца игры --- *)

let rec game_over_thread () =
  if !game_over then Lwt.return_unit
  else
    let%lwt () = Lwt_js.sleep 0.3 in
    if !game_over then Lwt.return_unit
    else begin
      if healthy_count () = 0 then begin
        game_over := true;
        show_game_over_overlay ();
        Lwt.return_unit
      end else
        game_over_thread ()
    end

(* --- репродукция крипов --- *)

let rec reproduction_thread () =
  if !game_over then Lwt.return_unit
  else
    let%lwt () = Lwt_js.sleep reproduction_interval in
    if !game_over then Lwt.return_unit
    else begin
      if any_healthy () then add_creep_from_reproduction ();
      reproduction_thread ()
    end

(* --- инициализация игры --- *)

let start_game () =
  Random.self_init ();
  (* получаем game div *)
  let game_div =
    match Html.getElementById "game"
          |> Html.CoerceTo.div
          |> Js.Opt.to_option with
    | None -> failwith "No #game div"
    | Some d -> d
  in
  game_div_ref := Some game_div;
  (* размеры области *)
  game_width := float_of_int game_div##.offsetWidth;
  game_height := float_of_int game_div##.offsetHeight;

  (* создаём стартовых крипов *)
  let created =
    List.init initial_creets (fun _ -> make_creep game_div)
  in
  creeps := created;

  (* каждому крипу — drag-обработчики и отдельный поток *)
  List.iter
    (fun c ->
       init_drag c;
       Lwt.async (fun () -> creep_thread c))
    created;

  (* потоки репродукции и проверки конца игры *)
  Lwt.async reproduction_thread;
  Lwt.async game_over_thread;
  ()

let () =
  Eliom_client.onload (fun () ->
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
               [txt "H42N42 — симуляция заражённых существ"];

             div
               ~a:[a_id "game-container";
                   a_style
                     "position:relative;\
                      width:800px;\
                      height:500px;\
                      margin:30px auto;\
                      background:#111;\
                      border:2px solid #555;\
                      box-shadow:0 0 15px rgba(0,0,0,0.6);\
                      overflow:hidden;"]
               [
                 (* всё поле целиком *)
                 div
                   ~a:[a_id "game";
                       a_style
                         "position:absolute;\
                          left:0; top:0;\
                          width:100%; height:100%;\
                          overflow:hidden;"]
                   [
                     (* река внутри игрового поля *)
                     div
                       ~a:[a_id "river";
                           a_style
                             "position:absolute;\
                              left:0; top:0;\
                              width:100%; height:80px;\
                              background:#2e8bff;\
                              opacity:0.5;\
                              pointer-events:none;"]
                       [txt "Токсичная река"];

                     (* больница внутри того же поля *)
                     div
                       ~a:[a_id "hospital";
                           a_style
                             "position:absolute;\
                              left:0; bottom:0;\
                              width:100%; height:80px;\
                              background:#4caf50;\
                              opacity:0.5;\
                              pointer-events:none;"]
                       [txt "Больница"];
                   ];
               ];
          ])))
