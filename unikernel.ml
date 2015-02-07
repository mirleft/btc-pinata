open Lwt
open V1
open V1_LWT

module Http = struct

  let headers status xs = 
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines   = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

end

module Main (C : CONSOLE) (S : STACKV4) (KV : KV_RO) (E : ENTROPY) (Clock : CLOCK) =
struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP) (E)
  module X509 = Tls_mirage.X509 (KV) (Clock)
  module KV   = Kv_ro_plus.Make (KV)

  module L = Logger.Make (C) (S) (Clock)

  let tls_accept ~tag ?(trace=false) c stack cfg tcp ~f =
    let peer = TCP.get_dest tcp in
    let log  = L.log_ext tag peer in
    let with_tls_server k =
      match trace with
      | true ->
          L.tracing peer @@ fun trace ->
            TLS.server_of_flow ~trace cfg tcp >>= k
      | false -> TLS.server_of_flow cfg tcp >>= k
    in
    with_tls_server @@ function
    | `Error _ -> log "TLS failed" ; TCP.close tcp
    | `Ok tls  -> log "TLS ok"     ; f tls >> TLS.close tls

  let tls_connect ~tag c stack cfg addr ~f =
    let log = L.log_ext tag addr in
    TCP.create_connection (S.tcpv4 stack) addr
    >>= function
    | `Error _ -> log "connection failed" ; return_unit
    | `Ok tcp  ->
        L.tracing addr @@ fun trace ->
          TLS.client_of_flow ~trace cfg "" tcp
          >>= function
          | `Error _ -> log "TLS failed" ; TCP.close tcp
          | `Ok tls  -> log "TLS ok"     ; f tls >> TLS.close tls

  let h_as_server c stack secret cfg =
    tls_accept ~trace:true ~tag:"server" c stack cfg
      ~f:(fun tls -> TLS.write tls secret)

  let h_as_client c stack secret cfg tcp =
    let (ip, port) as addr = TCP.get_dest tcp in
    L.log_ext "client" addr "received TCP" ;
    TCP.close tcp >>
    tls_connect ~tag:"client" c stack cfg (ip, 40001)
      ~f:(fun tls -> TLS.write tls secret)

  let h_as_rev_client c stack secret cfg tcp =
    let peer = TCP.get_dest tcp in
    let log  = L.log_ext "rev-client" peer in
    log "received TCP" ;
    L.tracing peer @@ fun trace ->
      TLS.client_of_flow ~trace cfg "" tcp
      >>= function
      | `Error _ -> log "TLS failed" ; TCP.close tcp
      | `Ok tls  -> log "TLS ok"     ; TLS.write tls secret >> TLS.close tls

  let http_header ~status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines   = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

  let header = http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("content-type", "text/html; charset=UTF-8") ]

  let h_notice _ data =
    fun tcp ->
      let log = L.log_ext "web" (TCP.get_dest tcp) in
      TCP.writev tcp [ header; data ] >>= function
      | `Error _ -> log "write error" ; TCP.close tcp
      | _        -> log "responded"   ; TCP.close tcp

  let h_as_web_server c stack data cfg =
    tls_accept ~tag:"web-server" c stack cfg
      ~f:(fun tls -> TLS.writev tls [ header; data ] )

  let tls_init e kv =
    lwt () = TLS.attach_entropy e in
    lwt authenticator    = X509.authenticator kv `CAs
    and no_authenticator = X509.authenticator kv `Noop
    and cert             = X509.certificate kv `Default in
    return Tls.Config.(
      server ~authenticator:no_authenticator ~certificates:(`Single cert) (),
      server ~authenticator ~certificates:(`Single cert) (),
      client ~authenticator ()
    )

  let start con stack kv e _ =
    L.init con stack ;
    lwt (w_cfg, s_cfg, c_cfg) = tls_init e kv
    and secret                = KV.reads_exn kv "secret"
    and ca_root               = KV.reads_exn kv "tls/ca-roots.crt" in
    let web_data              = Page.render ca_root in
    S.listen_tcpv4 stack ~port:8080  (h_notice con web_data) ;
    S.listen_tcpv4 stack ~port:4433  (h_as_web_server con stack web_data w_cfg) ;
    S.listen_tcpv4 stack ~port:10000 (h_as_server con stack secret s_cfg) ;
    S.listen_tcpv4 stack ~port:10001 (h_as_client con stack secret c_cfg) ;
    S.listen_tcpv4 stack ~port:10002 (h_as_rev_client con stack secret c_cfg) ;
    S.listen stack

end
