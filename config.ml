open Mirage

let net =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~arp:farp default_network)

let secret_k =
  let doc = Key.Arg.info ~doc:"Secret" ["s"; "secret"] in
  Key.(create "secret" Arg.(opt string ".oO( SEKRIT )Oo." doc))

let test_k =
  let doc = Key.Arg.info ~doc:"test mode" ["test"] in
  Key.(create "test" Arg.(flag doc))

let logger = syslog_udp ~config:(syslog_config ~truncate:1484 "pinata") net

let () =
  let keys = Key.([ abstract secret_k ; abstract test_k ])
  and packages = [
    package ~sublibs:["mirage"] "tls";
    package "tyxml";
    package "logs";
    package "ptime"]
  in
  register "btc-piñata" [
    foreign
      ~deps:[ abstract nocrypto ; abstract logger ; abstract app_info ]
      ~keys
      ~packages
      "Unikernel.Main"
      (stackv4 @-> pclock @-> job)
      $ net
      $ default_posix_clock
  ]
