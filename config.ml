open Mirage

let net =
  match get_mode () with
  | `Xen  -> direct_stackv4_with_dhcp default_console tap0
  | `Unix -> direct_stackv4_with_dhcp default_console (netif "tap9")


let kv = crunch "disk"

let () =
  add_to_ocamlfind_libraries ["tls.mirage"; "cow"; "cow.syntax"] ;
  register "btc-piñata" [
    foreign "Unikernel.Main"
      ( console @-> stackv4 @-> kv_ro @-> entropy @-> clock @-> job )
      $ default_console
      $ net
      $ kv
      $ default_entropy
      $ default_clock
  ]
