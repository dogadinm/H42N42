# H42N42 (Eliom / js_of_ocaml / Lwt)

This project is an implementation of the **H42N42** game using **Eliom**.
The game logic runs on the client side (compiled to JavaScript via **js_of_ocaml**) and uses **Lwt** cooperative threads for timing and concurrency.

## Features (Subject Checklist)

- **Statically validated HTML**:
  - The main page is generated with **TyXML** (`Eliom_content.Html.F`), so invalid HTML structures are rejected at compile time.
  - Dynamic creature nodes are also created with TyXML (`Eliom_content.Html.D`) and converted safely to DOM.

- **DOM interaction from OCaml**:
  - The OCaml client code directly manipulates the browser DOM:
    - updates `style.left/top/width/height`
    - changes colors by state
    - creates/removes DOM nodes
    - uses browser methods like `appendChild`, `removeChild`, `getBoundingClientRect`
  - Mouse events are handled using `Lwt_js_events`.

- **One Lwt thread per creature** (**required**):
  - Each creature runs in its own `creep_thread` (started with `Lwt.async`).
  - Additional global threads manage reproduction, elapsed time, and game-over detection.

- **Progressive difficulty**:
  - Creature speed increases gradually over time using an `elapsed_time`-based speed factor.

- **Reproduction**:
  - While at least one **Healthy** creature exists, a new creature spawns periodically.

## Game Rules Summary

### States
- **Healthy** (green): normal speed.
- **Sick** (red): slower movement; may evolve over time.
- **Berserk** (magenta): grows by +10% diameter every 10 seconds; dies when reaching 4× base size.
- **Mean** (orange): smaller (85%); actively chases the nearest Healthy; dies after 60 seconds.

### Infection
- Touching the **toxic river** (top zone) instantly infects a Healthy creature.
- Contact with an infected creature has a per-tick infection chance while overlapping.
- Dragged creatures are **invulnerable** to contamination.

### Healing
- The **hospital** (bottom zone) heals **only Sick** creatures, **only if the player manually drags and drops them** into the hospital.
- Berserk and Mean creatures cannot be grabbed or healed.

### Game Over
- The game ends when there are **no Healthy creatures remaining** (i.e., all remaining living creatures are contaminated).

## Controls

- **Drag & Drop (mouse)**:
  - Click and drag a **Healthy** or **Sick** creature.
  - Drop a **Sick** creature into the hospital to heal it.
  - Dragging allows moving a creature anywhere (no clamping during drag).

## Project Structure

- `h42n42.eliom`
  - Server side:
    - registers the main page and static resources (CSS)
  - Client side:
    - creature logic, movement, infection, evolution, reproduction
    - DOM updates and event handling

- `static/css/h42n42.css` (or `css/h42n42.css` depending on your setup)
  - page/game container styling

## Build & Run

### Prerequisites
- OCaml + opam
- Eliom + Ocsigenserver
- js_of_ocaml
- dune

(Exact versions depend on your opam switch and project constraints.)

### Build
```bash
docker-compose up --build
