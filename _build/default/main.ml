open Js_of_ocaml
open Js_of_ocaml_lwt

let document = Dom_html.document
let world = Dom_html.getElementById_exn "world"

(* Состояние каждого Creet *)
type creet_state = {
  mutable x: int;
  mutable y: int;
  mutable dx: int;
  mutable dy: int;
  mutable infected: bool;
  mutable berserk: bool;
  mutable mean: bool;
  mutable size: int;
  mutable is_being_dragged: bool;
  mutable invulnerable: bool;
}

(* Все Creet'ы *)
let creets : (Dom_html.divElement Js.t * creet_state) list ref = ref []

(* Создание DOM-элемента *)
let create_creet color x y size =
  let d = Dom_html.createDiv document in
  d##.className := Js.string "creet";
  d##.style##.backgroundColor := Js.string color;
  d##.style##.position := Js.string "absolute";
  d##.style##.width := Js.string (string_of_int size ^ "px");
  d##.style##.height := Js.string (string_of_int size ^ "px");
  d##.style##.borderRadius := Js.string "50%";
  d##.style##.left := Js.string (string_of_int x ^ "px");
  d##.style##.top := Js.string (string_of_int y ^ "px");
  Dom.appendChild world d;
  d

(* Проверка соприкосновения *)
let is_touching st1 st2 =
  let r1 = st1.size / 2 and r2 = st2.size / 2 in
  let dx = (st1.x + r1) - (st2.x + r2) in
  let dy = (st1.y + r1) - (st2.y + r2) in
  let dist2 = dx * dx + dy * dy in
  dist2 < (r1 + r2) * (r1 + r2)

(* Berserk растёт постепенно *)
let berserk_growth d st =
  let initial_size = st.size in
  let target_size = initial_size * 4 in
  let rec loop () =
    if st.berserk && st.size < target_size then (
      st.size <- st.size + 1;
      d##.style##.width := Js.string (string_of_int st.size ^ "px");
      d##.style##.height := Js.string (string_of_int st.size ^ "px");
      let%lwt () = Lwt_js.sleep 0.2 in
      loop ()
    ) else Lwt.return_unit
  in
  loop ()

(* Mean преследует ближайших здоровых *)
let find_closest_healthy st =
  let best = ref None and min_d = ref max_int in
  List.iter (fun (_, other) ->
    if not other.infected && not other.is_being_dragged then (
      let dx = other.x - st.x and dy = other.y - st.y in
      let d = dx * dx + dy * dy in
      if d < !min_d then (min_d := d; best := Some other)
    )
  ) !creets;
  !best

let mean_chase st =
  match find_closest_healthy st with
  | Some t ->
      let dx = t.x - st.x and dy = t.y - st.y in
      if dx <> 0 then st.dx <- if dx > 0 then 2 else -2;
      if dy <> 0 then st.dy <- if dy > 0 then 2 else -2;
  | None -> ()

(* Заражение Creet'а *)
let infect_creet (d, st) =
  if not st.infected && not st.invulnerable && not st.is_being_dragged then (
    st.infected <- true;

    (* Заражённый замедляется на 15% *)
    st.dx <- int_of_float (float st.dx *. 0.85);
    st.dy <- int_of_float (float st.dy *. 0.85);
    if st.dx = 0 then st.dx <- 1;
    if st.dy = 0 then st.dy <- 1;

    d##.style##.backgroundColor := Js.string "red";

    let r = Random.float 1.0 in
    if r < 0.10 then (
      (* Berserk *)
      st.berserk <- true;
      st.mean <- false;
      d##.style##.backgroundColor := Js.string "purple";
      Lwt.async (fun () -> berserk_growth d st)
    )
    else if r < 0.20 then (
      (* Mean *)
      st.mean <- true;
      st.berserk <- false;
      st.size <- int_of_float (float st.size *. 0.85);
      d##.style##.backgroundColor := Js.string "orange";
      d##.style##.width := Js.string (string_of_int st.size ^ "px");
      d##.style##.height := Js.string (string_of_int st.size ^ "px");
    )
  )

(* Попытка заразить других *)
let try_infect_others (_, st) =
  if st.infected then (
    List.iter (fun (d2, st2) ->
      if not st2.infected && not st2.invulnerable && not st2.is_being_dragged then
        if is_touching st st2 && Random.float 1.0 < 0.02 then
          infect_creet (d2, st2)
    ) !creets
  )

(* Автоматическое движение *)
let rec move_creet d st =
  if not st.is_being_dragged then (
    if st.mean then mean_chase st;

    (* случайная смена направления *)
    if Random.float 1.0 < 0.01 then (
      st.dx <- (Random.int 5) - 2;
      st.dy <- (Random.int 5) - 2;
    );

    let w = world##.clientWidth and h = world##.clientHeight in
    let nx = st.x + st.dx and ny = st.y + st.dy in
    let dx, nx =
      if nx < 0 || nx > w - st.size then (-st.dx, max 0 (min (w - st.size) nx))
      else (st.dx, nx)
    in
    let dy, ny =
      if ny < 0 || ny > h - st.size then (-st.dy, max 0 (min (h - st.size) ny))
      else (st.dy, ny)
    in
    st.dx <- dx; st.dy <- dy; st.x <- nx; st.y <- ny;

    (* заражение при входе в верхнюю зону *)
    if st.y < 50 && not st.infected && not st.invulnerable then
      infect_creet (d, st);

    (* лечение при опускании вниз, только если перетащен пользователем *)
    if st.infected && st.is_being_dragged && st.y > h - 70 then (
      st.infected <- false;
      st.berserk <- false;
      st.mean <- false;
      st.size <- 20;
      d##.style##.backgroundColor := Js.string "lime";
      d##.style##.width := Js.string "20px";
      d##.style##.height := Js.string "20px";
    );

    d##.style##.left := Js.string (string_of_int st.x ^ "px");
    d##.style##.top := Js.string (string_of_int st.y ^ "px");
  );

  try_infect_others (d, st);
  let delay = if st.infected then 0.05 else 0.03 in
  let%lwt () = Lwt_js.sleep delay in
  move_creet d st

(* Перетаскивание *)
let make_draggable d st =
  let open Lwt_js_events in
  let offset_x = ref 0 and offset_y = ref 0 in
  let world_rect = ref (world##getBoundingClientRect) in

  Lwt.async (fun () ->
    mousedowns d (fun ev _ ->
      st.is_being_dragged <- true;
      st.invulnerable <- true;
      world_rect := world##getBoundingClientRect;
      let rect = d##getBoundingClientRect in
      offset_x := int_of_float (float_of_int ev##.clientX -. rect##.left);
      offset_y := int_of_float (float_of_int ev##.clientY -. rect##.top);
      Lwt.return_unit)
  );

  Lwt.async (fun () ->
    mouseups document (fun _ _ ->
      if st.is_being_dragged then (
        st.is_being_dragged <- false;
        st.invulnerable <- false;
        (* Проверим, что отпущен в зоне хила *)
        let h = world##.clientHeight in
        if st.infected && st.y > h - 70 then (
          st.infected <- false;
          st.berserk <- false;
          st.mean <- false;
          st.size <- 20;
          d##.style##.backgroundColor := Js.string "lime";
          d##.style##.width := Js.string "20px";
          d##.style##.height := Js.string "20px";
        )
      );
      Lwt.return_unit)
  );

  Lwt.async (fun () ->
    mousemoves document (fun ev _ ->
      if st.is_being_dragged then (
        let wr = !world_rect in
        let nx = int_of_float (float_of_int ev##.clientX -. wr##.left) - !offset_x in
        let ny = int_of_float (float_of_int ev##.clientY -. wr##.top) - !offset_y in
        st.x <- nx;
        st.y <- ny;
        d##.style##.left := Js.string (string_of_int nx ^ "px");
        d##.style##.top := Js.string (string_of_int ny ^ "px");
      );
      Lwt.return_unit)
  );
  Lwt.return_unit

(* Добавление нового Creet *)
let add_creet color x y =
  let size = 20 in
  let d = create_creet color x y size in
  let st = {
    x; y;
    dx = (let v = (Random.int 5) - 2 in if v = 0 then 1 else v);
    dy = (let v = (Random.int 5) - 2 in if v = 0 then 1 else v);
    infected = false;
    berserk = false;
    mean = false;
    size;
    is_being_dragged = false;
    invulnerable = false;
  } in
  creets := (d, st) :: !creets;
  Lwt.async (fun () -> move_creet d st);
  Lwt.async (fun () -> make_draggable d st)

(* GAME OVER — когда не осталось здоровых *)
let display_game_over () =
  let msg = Dom_html.createDiv document in
  msg##.innerHTML := Js.string "<h1>GAME OVER</h1>";
  msg##.style##.position := Js.string "absolute";
  msg##.style##.top := Js.string "50%";
  msg##.style##.left := Js.string "50%";
  msg##.style##.transform := Js.string "translate(-50%, -50%)";
  msg##.style##.fontSize := Js.string "60px";
  msg##.style##.padding := Js.string "20px";
  msg##.style##.color := Js.string "white";
  msg##.style##.background := Js.string "rgba(0,0,0,0.7)";
  msg##.style##.border := Js.string "3px solid white";
  msg##.style##.borderRadius := Js.string "12px";
  Dom.appendChild world msg

let check_game_over () =
  let healthy_exists = List.exists (fun (_, st) -> not st.infected) !creets in
  if not healthy_exists then (
    display_game_over ();
    List.iter (fun (_, st) -> st.is_being_dragged <- true) !creets;
    Lwt.fail Exit   (* Останавливаем циклы *)
  ) else
    Lwt.return_unit

(* Размножение *)
let rec reproduction_loop () =
  let%lwt () = check_game_over () in

  let healthy_exists = List.exists (fun (_, st) -> not st.infected) !creets in
  if healthy_exists && Random.float 1.0 < 0.3 then (
    let x = Random.int (world##.clientWidth - 30) in
    let y = Random.int (world##.clientHeight - 30) in
    add_creet "lime" x y;
  );

  let%lwt () = Lwt_js.sleep 2.0 in
  reproduction_loop ()

(* Постепенное ускорение всех Creets *)
let rec difficulty_loop () =
  List.iter (fun (_, st) ->
    if not st.is_being_dragged then (
      st.dx <- st.dx + (if st.dx >= 0 then 1 else -1);
      st.dy <- st.dy + (if st.dy >= 0 then 1 else -1)
    )
  ) !creets;

  (* Каждые 12 секунд игра становится немного сложнее *)
  let%lwt () = Lwt_js.sleep 12.0 in
  difficulty_loop ()

(* main *)
let () =
  Random.self_init ();
  add_creet "lime" 200 200;
  Lwt.async reproduction_loop;
  Lwt.async difficulty_loop