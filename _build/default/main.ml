open Js_of_ocaml
open Js_of_ocaml_lwt

let document = Dom_html.document
let world = Dom_html.getElementById_exn "world"

(* Общее состояние для одного "крита", чтобы функции могли обмениваться данными *)
type creet_state = {
  mutable x: int;
  mutable y: int;
  mutable dx: int;
  mutable dy: int;
  mutable infected: bool;
  mutable is_being_dragged: bool; (* Флаг, показывающий, что объект перетаскивают *)
}

(* Создание нового Creet *)
let create_creet color x y =
  let d = Dom_html.createDiv document in
  d##.className := Js.string "creet";
  d##.style##.backgroundColor := Js.string color;
  d##.style##.left := Js.string (string_of_int x ^ "px");
  d##.style##.top := Js.string (string_of_int y ^ "px");
  Dom.appendChild world d;
  d

(* Функция автоматического движения *)
let rec move_creet d state =
  (* Двигаемся, только если нас не перетаскивают вручную *)
  if not state.is_being_dragged then (
    let w = world##.clientWidth and h = world##.clientHeight in
    let nx = state.x + state.dx and ny = state.y + state.dy in

    (* Отражение от стен *)
    let dx, nx =
      if nx < 0 || nx > w - 20 then (-state.dx, max 0 (min (w - 20) nx)) else (state.dx, nx)
    in
    let dy, ny =
      if ny < 0 || ny > h - 20 then (-state.dy, max 0 (min (h - 20) ny)) else (state.dy, ny)
    in
    
    (* Обновляем общее состояние *)
    state.dx <- dx;
    state.dy <- dy;
    state.x <- nx;
    state.y <- ny;

    (* --- ЗАРАЖЕНИЕ И ЛЕЧЕНИЕ --- *)
    let infected =
      if state.y < 50 && not state.infected then (
        d##.style##.backgroundColor := Js.string "red"; true
      ) else if state.infected && state.y > h - 70 then (
        d##.style##.backgroundColor := Js.string "lime"; false
      ) else state.infected
    in
    state.infected <- infected;

    (* Обновление позиции в DOM *)
    d##.style##.left := Js.string (string_of_int state.x ^ "px");
    d##.style##.top := Js.string (string_of_int state.y ^ "px");
  );

  (* Пауза перед следующим шагом *)
  let delay = if state.infected then 0.05 else 0.03 in
  let%lwt () = Lwt_js.sleep delay in
  move_creet d state


(* Функция для перетаскивания *)
let make_draggable d state =
  let open Lwt_js_events in
  let offset_x = ref 0 and offset_y = ref 0 in
  (* Используем ref для world_rect, чтобы обновлять его при каждом нажатии *)
  let world_rect = ref (world##getBoundingClientRect) in

  (* FIX: Запускаем каждый обработчик событий асинхронно, чтобы они работали параллельно, а не последовательно. *)

  (* Нажатие кнопки мыши: начинаем перетаскивание *)
  Lwt.async (fun () ->
    mousedowns d (fun ev _ ->
        state.is_being_dragged <- true;
        world_rect := world##getBoundingClientRect; (* Обновляем геометрию на случай сдвига страницы *)
        let rect = d##getBoundingClientRect in
        offset_x := int_of_float (float_of_int ev##.clientX -. rect##.left);
        offset_y := int_of_float (float_of_int ev##.clientY -. rect##.top);
        Lwt.return_unit)
  );

  (* Отпускание кнопки мыши: заканчиваем перетаскивание *)
  Lwt.async (fun () ->
    mouseups document (fun _ _ ->
        state.is_being_dragged <- false;
        Lwt.return_unit)
  );
  
  (* Движение мыши *)
  Lwt.async (fun () ->
    mousemoves document (fun ev _ ->
        if state.is_being_dragged then (
          let wr = !world_rect in
          let nx = int_of_float (float_of_int ev##.clientX -. wr##.left) - !offset_x in
          let ny = int_of_float (float_of_int ev##.clientY -. wr##.top) - !offset_y in
          
          (* Обновляем и состояние, и позицию в DOM *)
          state.x <- nx;
          state.y <- ny;
          d##.style##.left := Js.string (string_of_int nx ^ "px");
          d##.style##.top := Js.string (string_of_int ny ^ "px");
          
          (* Выводим в консоль для проверки *)
          Firebug.console##log(Js.string (Printf.sprintf "Dragging to: x=%d, y=%d" nx ny));
        );
        Lwt.return_unit)
  );

  Lwt.return_unit (* Функция настройки возвращается сразу *)

(* Главная функция *)
let () =
  let c = create_creet "lime" 200 200 in
  (* Создаем начальное состояние объекта *)
  let initial_state = {
    x = 200;
    y = 200;
    dx = 3;
    dy = 2;
    infected = false;
    is_being_dragged = false;
  } in
  (* Запускаем обе асинхронные функции (движение и перетаскивание) параллельно *)
  Lwt.async (fun () -> move_creet c initial_state);
  Lwt.async (fun () -> make_draggable c initial_state);
  ()