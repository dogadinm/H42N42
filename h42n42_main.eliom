(** This is the main file if you are using static linking without config file.
 *)

module%shared H42n42 = H42n42

let%server _ =
  Ocsigen_server.start
    ~ports:[`All, 8080]
    ~veryverbose:()
    ~debugmode:true
    ~logdir:"local/var/log/h42n42"
    ~datadir:"local/var/data/h42n42"
    ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true
    ~command_pipe:"local/var/run/h42n42-cmd"
    ~default_charset:(Some "utf-8")
    [ Ocsigen_server.host
      [Staticmod.run ~dir:"local/var/www/h42n42" (); Eliom.run ()] ]
