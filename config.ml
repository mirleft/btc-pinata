open Mirage

let net = generic_stackv4 default_network

let secret_k =
  let doc = Key.Arg.info ~doc:"Secret" ["s"; "secret"] in
  Key.(create "secret" Arg.(opt string ".oO( SEKRIT )Oo." doc))

let test_k =
  let doc = Key.Arg.info ~doc:"test mode" ["test"] in
  Key.(create "test" Arg.(flag doc))

let monitor =
  let doc = Key.Arg.info ~doc:"monitor host IP" ["monitor"] in
  Key.(create "monitor" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let syslog =
  let doc = Key.Arg.info ~doc:"syslog host IP" ["syslog"] in
  Key.(create "syslog" Arg.(opt ipv4_address Ipaddr.V4.unspecified doc))

let name =
  let doc = Key.Arg.info ~doc:"Name of the unikernel" ["name"] in
  Key.(create "name" Arg.(opt string "pinata" doc))

let management_stack = generic_stackv4 ~group:"management" (netif ~group:"management" "management")

let () =
  let keys = [
    Key.abstract secret_k ; Key.abstract test_k ;
    Key.abstract name ; Key.abstract syslog ; Key.abstract monitor
  ]
  and packages = [
    package ~sublibs:["mirage"] "tls";
    package "tyxml";
    package "logs";
    package "ptime";
    package "monitoring-experiments";
    package ~sublibs:["mirage"] "logs-syslog";
  ]
  in
  register "btc-piñata" [
    foreign
      ~deps:[ abstract nocrypto ; abstract app_info ]
      ~keys
      ~packages
      "Unikernel.Main"
      (console @-> time @-> mclock @-> pclock @-> stackv4 @-> stackv4 @-> job)
      $ default_console $ default_time $ default_monotonic_clock $ default_posix_clock $ net $ management_stack
  ]
