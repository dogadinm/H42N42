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

(* Проверка пересечения двух Creet'ов *)
let is_touching st1 st2 =
  let size = 20 in
  abs (st1.x - st2.x) < size && abs (st1.y - st2.y) < size

(* Попытка заразить других *)
let try_infect_others (d, st) =
  let _ = d in
  if st.infected then (
    List.iter (fun (_, other) ->
      if not other.infected && not other.is_being_dragged then (
        if is_touching st other && Random.float 1.0 < 0.02 then (
          other.infected <- true;
        )
      )
    ) !creets
  )

(* Случайная небольшая смена направления *)
let maybe_change_direction state =
  if Random.float 1.0 < 0.01 then (  (* примерно 1% шанс на тик *)
    let new_dx = (Random.int 5) - 2 in
    let new_dy = (Random.int 5) - 2 in
    if new_dx <> 0 then state.dx <- new_dx;
    if new_dy <> 0 then state.dy <- new_dy;
  )
  else if state.dx = 0 || state.dy = 0 then (  (* страховка от “залипания” *)
    state.dx <- if state.dx = 0 then (if Random.bool () then 1 else -1) else state.dx;
    state.dy <- if state.dy = 0 then (if Random.bool () then 1 else -1) else state.dy;
  )

(* Автоматическое движение *)
let rec move_creet d state =
  if not state.is_being_dragged then (
    let w = world##.clientWidth and h = world##.clientHeight in

    (* Редко меняем направление *)
    maybe_change_direction state;

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
    );

    (* Попытка заразить других при контакте *)
    try_infect_others (d, state);

    (* Обновляем цвет в соответствии с состоянием *)
    d##.style##.backgroundColor :=
      Js.string (if state.infected then "red" else "lime");

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
      Lwt.return_unit)
  );

  (* Отпускание мыши — закончить перетаскивание и возможно вылечить *)
  Lwt.async (fun () ->
    mouseups document (fun _ _ ->
      let h = world##.clientHeight in
      if state.is_being_dragged && state.infected && state.y > h - 70 then (
        state.infected <- false;
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
