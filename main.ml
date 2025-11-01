open Js_of_ocaml
open Js_of_ocaml_lwt

let document = Dom_html.document
let world = Dom_html.getElementById_exn "world"

(* Состояние существа *)
type creet_state = {
  mutable x: int;
  mutable y: int;
  mutable dx: int;
  mutable dy: int;
  mutable infected: bool;
  mutable is_being_dragged: bool;
}

(* Все Creet'ы *)
let creets : (Dom_html.divElement Js.t * creet_state) list ref = ref []

(* Создание DOM-элемента *)
let create_creet color x y =
  let d = Dom_html.createDiv document in
  d##.className := Js.string "creet";
  d##.style##.backgroundColor := Js.string color;
  d##.style##.left := Js.string (string_of_int x ^ "px");
  d##.style##.top := Js.string (string_of_int y ^ "px");
  Dom.appendChild world d;
  d

(* Автоматическое движение *)
let rec move_creet d state =
  if not state.is_being_dragged then (
    let w = world##.clientWidth and h = world##.clientHeight in
    let nx = state.x + state.dx and ny = state.y + state.dy in

    let dx, nx =
      if nx < 0 || nx > w - 20 then (-state.dx, max 0 (min (w - 20) nx))
      else (state.dx, nx)
    in
    let dy, ny =
      if ny < 0 || ny > h - 20 then (-state.dy, max 0 (min (h - 20) ny))
      else (state.dy, ny)
    in

    state.dx <- dx;
    state.dy <- dy;
    state.x <- nx;
    state.y <- ny;

    (* Заражение в верхней зоне — только если не перетаскивается *)
    if state.y < 50 && not state.infected && not state.is_being_dragged then (
      state.infected <- true;
      d##.style##.backgroundColor := Js.string "red"
    );

    (* Больница внизу больше не лечит автоматически *)
    d##.style##.left := Js.string (string_of_int state.x ^ "px");
    d##.style##.top := Js.string (string_of_int state.y ^ "px");
  );

  let delay = if state.infected then 0.05 else 0.03 in
  let%lwt () = Lwt_js.sleep delay in
  move_creet d state

(* Перетаскивание *)
let make_draggable d state =
  let open Lwt_js_events in
  let offset_x = ref 0 and offset_y = ref 0 in
  let world_rect = ref (world##getBoundingClientRect) in

  (* Нажатие мыши — начать перетаскивание *)
  Lwt.async (fun () ->
    mousedowns d (fun ev _ ->
      state.is_being_dragged <- true;
      world_rect := world##getBoundingClientRect;
      let rect = d##getBoundingClientRect in
      offset_x := int_of_float (float_of_int ev##.clientX -. rect##.left);
      offset_y := int_of_float (float_of_int ev##.clientY -. rect##.top);
      (* Во время перетаскивания Creet невосприимчив к заражению *)
      Lwt.return_unit)
  );

  (* Отпускание мыши — закончить перетаскивание и возможно вылечить *)
  Lwt.async (fun () ->
    mouseups document (fun _ _ ->
      let h = world##.clientHeight in
      if state.is_being_dragged && state.infected && state.y > h - 70 then (
        state.infected <- false;
        d##.style##.backgroundColor := Js.string "lime"
      );
      state.is_being_dragged <- false;
      Lwt.return_unit)
  );

  (* Движение мыши — обновляем позицию *)
  Lwt.async (fun () ->
    mousemoves document (fun ev _ ->
      if state.is_being_dragged then (
        let wr = !world_rect in
        let nx = int_of_float (float_of_int ev##.clientX -. wr##.left) - !offset_x in
        let ny = int_of_float (float_of_int ev##.clientY -. wr##.top) - !offset_y in
        state.x <- nx;
        state.y <- ny;
        d##.style##.left := Js.string (string_of_int nx ^ "px");
        d##.style##.top := Js.string (string_of_int ny ^ "px");
      );
      Lwt.return_unit)
  );

  Lwt.return_unit

(* Добавление нового Creet *)
let add_creet color x y =
  let d = create_creet color x y in
  let state = {
    x; y;
    dx = (let v = (Random.int 5) - 2 in if v = 0 then 1 else v);
    dy = (let v = (Random.int 5) - 2 in if v = 0 then 1 else v);
    infected = false;
    is_being_dragged = false;
  } in
  creets := (d, state) :: !creets;
  Lwt.async (fun () -> move_creet d state);
  Lwt.async (fun () -> make_draggable d state)

(* Размножение *)
let rec reproduction_loop () =
  let healthy_exists = List.exists (fun (_, st) -> not st.infected) !creets in
  if healthy_exists then (
    if Random.float 1.0 < 0.3 then (
      let x = Random.int (world##.clientWidth - 30) in
      let y = Random.int (world##.clientHeight - 30) in
      add_creet "lime" x y;
    )
  );
  let%lwt () = Lwt_js.sleep 2.0 in
  reproduction_loop ()

(* main *)
let () =
  Random.self_init ();
  add_creet "lime" 200 200;
  Lwt.async reproduction_loop
