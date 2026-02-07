(lang dune 3.14)

(name h42n42)

(package
 (name h42n42))

(dialect
 (name "eliom-server")
 (implementation (extension "eliom"))
 (interface (extension "eliomi")))

(wrapped_executables false)

(formatting (enabled_for ocaml "eliom-server"))