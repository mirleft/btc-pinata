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

  let generate_cert ?ca now cn extensions =
    let valid_from, valid_until = valid 365 now
    and key = rsa_key ()
    in
    let name =
      [ X509.Distinguished_name.(Relative_distinguished_name.singleton (CN cn)) ]
    in
    let req = X509.Signing_request.create name key in
    let issuer, cakey = match ca with
      | None -> name, key
      | Some (k, cert) -> X509.Certificate.subject cert, k
    in
    let cert =
      X509.Signing_request.sign req ~valid_from ~valid_until ~extensions
        cakey issuer
    in
    if Key_gen.test () then
      Logs.info (fun m -> m "generated key and cert %a:\n%s\n%s"
                    X509.Certificate.pp cert
                    (X509.Private_key.encode_pem key |> Cstruct.to_string)
                    (X509.Certificate.encode_pem cert |> Cstruct.to_string));
    key, cert

  let generate_certs now =
    let cakey, cacert =
      let extensions =
        X509.Extension.(add Basic_constraints (true, (true, Some 1))
                          (singleton Key_usage (true, [`Key_cert_sign])))
      in
      generate_cert now "BTC Piñata CA" extensions
    in
    let extensions eku =
      X509.Extension.(add Basic_constraints (true, (false, None))
                        (add Key_usage (true, [ `Digital_signature ; `Key_encipherment ])
                           (singleton Ext_key_usage (true, [ eku ]))))
    in
    let ca = cakey, cacert in
    (cacert,
     generate_cert ~ca now "BTC Piñata server" (extensions `Server_auth),
     generate_cert ~ca now "BTC Piñata client" (extensions `Client_auth),
     generate_cert ~ca now "ownme.ipredator.se" (extensions `Server_auth))

  let tls_init clock =
    let now = Ptime.v (CLOCK.now_d_ps clock) in
    let cacert, s_cert, c_cert, w_cert = generate_certs now in
    let to_tls (key, cert) = `Single ([cert;cacert], match key with `RSA k -> k) in
    let authenticator = X509.Authenticator.chain_of_trust ~time:now [cacert] in
    let cacert = X509.Certificate.encode_pem cacert in
    Tls.Config.(
      cacert,
      server ~authenticator ~certificates:(to_tls s_cert) (),
      client ~authenticator ~certificates:(to_tls c_cert) (),
      server ~certificates:(to_tls w_cert) ()
    )

  let start stack clock _ info =
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
