open Lwt
open Mirage_types_lwt

module Main (S : STACKV4) (CLOCK : PCLOCK) =
struct

  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)

  let prefix tag (ip, port) =
    Printf.sprintf "[%s] %s:%d" tag (Ipaddr.V4.to_string ip) port

  let log prefix msg =
    Logs.info (fun m -> m "%s %s" prefix msg)

  let tls_accept ~tag ?(trace=false) cfg tcp ~f =
    let pre = prefix tag (TCP.dst tcp) in
    let log = log pre in
    let with_tls_server k =
      match trace with
      | true ->
        let trace s = log (Sexplib.Sexp.to_string s) in
        TLS.server_of_flow ~trace cfg tcp >>= k
      | false -> TLS.server_of_flow cfg tcp >>= k
    in
    with_tls_server @@ function
    | Error e -> Logs.warn (fun f -> f "%s TLS failed %a" pre TLS.pp_write_error e) ; TCP.close tcp
    | Ok tls -> log "TLS ok" ; f tls >>= fun _ -> TLS.close tls

  let tls_connect ~tag stack cfg addr ~f =
    let pre = prefix tag addr in
    let log = log pre in
    TCP.create_connection (S.tcpv4 stack) addr >>= function
    | Error e -> Logs.warn (fun f -> f "%s connection failed %a" pre TCP.pp_error e) ; return_unit
    | Ok tcp  ->
      let trace s = log (Sexplib.Sexp.to_string s) in
      TLS.client_of_flow ~trace cfg tcp >>= function
      | Error e -> Logs.warn (fun f -> f "%s TLS failed %a" pre TLS.pp_write_error e) ; TCP.close tcp
      | Ok tls -> log "TLS ok" ; f tls >>= fun _ -> TLS.close tls

  let h_as_server secret cfg =
    tls_accept ~trace:true ~tag:"server" cfg
      ~f:(fun tls -> TLS.write tls secret)

  let h_as_client stack secret cfg tcp =
    let (ip, _) as addr = TCP.dst tcp in
    let tag = "client" in
    let pre = prefix tag addr in
    log pre "received TCP" ;
    TCP.close tcp >>= fun () ->
    tls_connect ~tag stack cfg (ip, 40001) ~f:(fun tls -> TLS.write tls secret)

  let h_as_rev_client secret cfg tcp =
    let pre     = prefix "rev-client" (TCP.dst tcp) in
    let log     = log pre in
    let trace s = log (Sexplib.Sexp.to_string s) in
    TLS.client_of_flow ~trace cfg tcp >>= function
    | Error e -> Logs.warn (fun f -> f "%s TLS failed %a" pre TLS.pp_write_error e) ; TCP.close tcp
    | Ok tls -> log "TLS ok" ; TLS.write tls secret >>= fun _ -> TLS.close tls

  let http_header ~status xs =
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines   = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

  let header len = http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("Content-Type", "text/html; charset=UTF-8") ;
        ("Content-lengt", string_of_int len) ;
        ("Connection", "close") ]

  let h_notice data tcp =
    let pre = prefix "web" (TCP.dst tcp) in
    let log = log pre in
    let len = Cstruct.len data in
    TCP.writev tcp [ header len; data ] >>= function
    | Error e -> Logs.warn (fun f -> f "%s tcp error %a" pre TCP.pp_write_error e) ; TCP.close tcp
    | Ok ()   -> log "responded" ; TCP.close tcp

  let h_as_web_server data cfg =
    let len = Cstruct.len data in
    tls_accept ~tag:"web-server" cfg
      ~f:(fun tls -> TLS.writev tls [ header len; data ] )

  let valid days now =
    match Ptime.(add_span now (Span.unsafe_of_d_ps (days, 0L))) with
    | Some expire -> (now, expire)
    | None -> assert false

  let rsa_key ?(bits = 4096) () =
    `RSA (Nocrypto.Rsa.generate bits)

  let generate_ca now () =
    let valid_from, valid_until = valid 365 now
    and cakey  = rsa_key ~bits:4096 ()
    and caname = [`CN "BTC Piñata CA"] in
    let req        = X509.CA.request caname cakey in
    let extensions = [(true, `Basic_constraints (true, Some 1));
                      (true, `Key_usage [`Key_cert_sign])]
    in
    (cakey,
     X509.CA.sign req ~valid_from ~valid_until ~extensions cakey caname)

  let generate_certs now (cakey, cacert) () =
    let valid_from, valid_until = valid 365 now
    and caname = X509.subject cacert in
    let gen_cert name extensions =
      let key  = rsa_key () in
      let req  = X509.CA.request [`CN name] key in
      let cert = X509.CA.sign req ~valid_from ~valid_until ~extensions cakey caname in
      ([cert; cacert], match key with `RSA k -> k)
    and extensions eku =
      [(true, `Key_usage [ `Digital_signature ; `Key_encipherment ]);
       (true, `Basic_constraints (false, None)) ;
       (true, `Ext_key_usage [eku])]
    in
    (cacert,
     gen_cert "BTC Piñata server" (extensions `Server_auth),
     gen_cert "BTC Piñata client" (extensions `Client_auth),
     gen_cert "ownme.ipredator.se" (extensions `Server_auth))

  let tls_init clock =
    let now = Ptime.v (CLOCK.now_d_ps clock) in
    let cacert, s_cert, c_cert, w_cert =
      if Key_gen.test () then
        generate_certs now Test.ca ()
      else
        generate_certs now (generate_ca now ()) ()
    in
    let authenticator = X509.Authenticator.chain_of_trust ~time:now [cacert] in
    let cacert = X509.Encoding.Pem.Certificate.to_pem_cstruct1 cacert in
    Tls.Config.(
      cacert,
      server ~authenticator ~certificates:(`Single s_cert) (),
      client ~authenticator ~certificates:(`Single c_cert) (),
      server ~certificates:(`Single w_cert) ()
    )

  let start stack clock _ _ info =
    Logs.info (fun m -> m "used packages: %a"
                  Fmt.(Dump.list @@ pair ~sep:(unit ".") string string)
                  info.Mirage_info.packages) ;
    Logs.info (fun m -> m "used libraries: %a"
                  Fmt.(Dump.list string) info.Mirage_info.libraries) ;
    let ca_root, s_cfg, c_cfg, w_cfg = tls_init clock in
    let secret = Cstruct.of_string (Key_gen.secret ()) in
    let web_data = Page.render ca_root in
    S.listen_tcpv4 stack ~port:80    (h_notice web_data) ;
    S.listen_tcpv4 stack ~port:443   (h_as_web_server web_data w_cfg) ;
    S.listen_tcpv4 stack ~port:10000 (h_as_server secret s_cfg) ;
    S.listen_tcpv4 stack ~port:10001 (h_as_client stack secret c_cfg) ;
    S.listen_tcpv4 stack ~port:10002 (h_as_rev_client secret c_cfg) ;
    S.listen stack
end
