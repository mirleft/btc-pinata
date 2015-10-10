open Mirage

let address addr nm gw =
  let f = Ipaddr.V4.of_string_exn in
  { address = f addr ; netmask = f nm ; gateways = [f gw] }

let server = address "198.167.222.202" "255.255.255.0" "198.167.222.1"

let net =
  match get_mode () with
  | `Unix -> direct_stackv4_with_default_ipv4 default_console (netif "tap0")
  | `Xen  -> direct_stackv4_with_dhcp default_console tap0
(*   | `Xen  -> direct_stackv4_with_static_ipv4 default_console tap0 server *)


let kv = crunch "disk"

let () =
  add_to_ocamlfind_libraries ["tls.mirage"; "cow"; "cow.syntax"; "ptime"] ;
  add_to_opam_packages ["tls"; "cow"; "ptime"] ;
  register "btc-piñata" [
    foreign "Unikernel.Main"
      ( console @-> stackv4 @-> kv_ro @-> clock @-> job )
      $ default_console
      $ net
      $ kv
      $ default_clock
  ]
