open Lwt
open V1_LWT

module Main (S : STACKV4) (KEYS : KV_RO) (CLOCK : V1.PCLOCK) =
struct

  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)
  module MX509 = Tls_mirage.X509 (KEYS) (CLOCK)

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

  let header = http_header
      ~status:"HTTP/1.1 200 OK"
      [ ("Content-Type", "text/html; charset=UTF-8") ;
        ("Connection", "close") ]

  let h_notice data tcp =
    let pre = prefix "web" (TCP.dst tcp) in
    let log = log pre in
    TCP.writev tcp [ header; data ] >>= function
    | Error e -> Logs.warn (fun f -> f "%s tcp error %a" pre TCP.pp_write_error e) ; TCP.close tcp
    | Ok ()   -> log "responded" ; TCP.close tcp

  let h_as_web_server data cfg =
    tls_accept ~tag:"web-server" cfg
      ~f:(fun tls -> TLS.writev tls [ header; data ] )

  let valid days now =
    let asn1_of_time time =
      let date, ((h, m, s), _) = Ptime.(to_date_time time) in
      {
        Asn.Time.date = date ;
        time = (h, m, s, 0.) ;
        tz = None;
      }
    in
    let start = asn1_of_time now in
    match Ptime.(add_span now (Span.unsafe_of_d_ps (days, 0L))) with
    | Some t ->
      let expire = asn1_of_time t in
      (start, expire)
    | None -> assert false

  let rsa_key ?(bits = 2048) () =
    `RSA (Nocrypto.Rsa.generate bits)

  let generate_ca now () =
    let valid_from, valid_until = valid 365 now
    and cakey  = rsa_key ~bits:4096 ()
    and caname = [`CN "BTC Pinata CA"] in
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
     gen_cert "Pinata server" (extensions `Server_auth),
     gen_cert "Pinata client" (extensions `Client_auth))

  let tls_init clock =
    let now = Ptime.v (CLOCK.now_d_ps clock) in
    let cacert, s_cert, c_cert =
      if Key_gen.test () then
        generate_certs now Test.ca ()
      else
        generate_certs now (generate_ca now ()) ()
    in
    let time = Ptime.to_float_s now in
    let authenticator = X509.Authenticator.chain_of_trust ~time [cacert] in
    let cacert = X509.Encoding.Pem.Certificate.to_pem_cstruct1 cacert in
    Tls.Config.(
      cacert,
      server ~authenticator ~certificates:(`Single s_cert) (),
      client ~authenticator ~certificates:(`Single c_cert) ()
    )

  let start stack keys clock _ _ =
    let ca_root, s_cfg, c_cfg = tls_init clock in
    MX509.certificate keys `Default >>= fun w_cert ->
    let w_cfg = Tls.Config.server ~certificates:(`Single w_cert) () in
    let secret = Cstruct.of_string (Key_gen.secret ()) in
    let web_data = Page.render ca_root in
    S.listen_tcpv4 stack ~port:80    (h_notice web_data) ;
    S.listen_tcpv4 stack ~port:443   (h_as_web_server web_data w_cfg) ;
    S.listen_tcpv4 stack ~port:10000 (h_as_server secret s_cfg) ;
    S.listen_tcpv4 stack ~port:10001 (h_as_client stack secret c_cfg) ;
    S.listen_tcpv4 stack ~port:10002 (h_as_rev_client secret c_cfg) ;
    S.listen stack
end
