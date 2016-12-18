open Mirage

let address =
  let network = Ipaddr.V4.Prefix.of_address_string_exn "198.167.222.202/24"
  and gateway = Ipaddr.V4.of_string "198.167.222.1"
  in
  { network ; gateway }

let net =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~config:address default_network)

let secret_k =
  let doc = Key.Arg.info ~doc:"Secret" ["s"; "secret"] in
  Key.(create "secret" Arg.(opt string ".oO( SEKRIT )Oo." doc))

let test_k =
  let doc = Key.Arg.info ~doc:"test mode" ["test"] in
  Key.(create "test" Arg.(flag doc))

let () =
  let keys = Key.([ abstract secret_k ; abstract test_k ])
  and packages = [
    package ~sublibs:["mirage"] "tls";
    package "tyxml";
    package "logs";
    package ~sublibs:["mirage"] "logs-syslog";
    package "ptime"]
  in
  register "btc-piñata" [
    foreign
      ~deps:[abstract nocrypto]
      ~keys
      ~packages
      "Unikernel.Main"
      (console @-> stackv4 @-> kv_ro @-> pclock @-> job)
      $ default_console
      $ net
      $ crunch "tls"
      $ default_posix_clock
  ]
