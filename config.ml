open Mirage

let address addr nm gw =
  let f = Ipaddr.V4.of_string_exn in
  { address = f addr ; netmask = f nm ; gateways = [f gw] }

let server = address "198.167.222.202" "255.255.255.0" "198.167.222.1"

let net =
  match get_mode () with
  | `Unix -> socket_stackv4 default_console [Ipaddr.V4.any]
  | `Xen  -> direct_stackv4_with_dhcp default_console tap0
(*   | `Xen  -> direct_stackv4_with_static_ipv4 default_console tap0 server *)

let secret_k =
  let doc = Key.Arg.info ~doc:"Secret" ["s"; "secret"] in
  Key.(create "secret" Arg.(opt string ".oO( SEKRIT )Oo." doc))

let test_k =
  let doc = Key.Arg.info ~doc:"test mode" ["test"] in
  Key.(create "test" Arg.(flag doc))

let () =
  let keys = Key.([ abstract secret_k ; abstract test_k ])
  and libraries = ["tls.mirage"; "tyxml"; "ptime"]
  and packages = ["tls"; "tyxml"; "ptime"]
  in
  register "btc-piñata" [
    foreign
      ~libraries
      ~deps:[abstract nocrypto]
      ~keys
      ~packages
      "Unikernel.Main"
      ( stackv4 @-> kv_ro @-> clock @-> job )
      $ net
      $ crunch "tls"
      $ default_clock
  ]
