open Mirage

let net = generic_stackv4 default_network

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
    package "ptime";
    package "monitoring-experiments";
  ]
  in
  register "btc-piñata" [
    foreign
      ~deps:[ abstract nocrypto ; abstract app_info ]
      ~keys
      ~packages
      "Unikernel.Main"
      (stackv4 @-> pclock @-> job)
      $ net
      $ default_posix_clock
  ]
