open Lwt.Infix

module Main (C : Mirage_console.S) (T : Mirage_time.S) (M : Mirage_clock.MCLOCK) (CLOCK : Mirage_clock.PCLOCK) (S : Mirage_stack.V4) (Management : Mirage_stack.V4) = struct
  module TCP   = S.TCPV4
  module TLS   = Tls_mirage.Make (TCP)

  let pinata_stats =
    Monitoring_experiments.counter_metrics ~f:(fun f -> f) "pinata"

  let trace_out tag sexp =
    Logs.info (fun m -> m "[%s]: %s" tag (Sexplib.Sexp.to_string sexp))

  let tls_accept ~tag ?(trace=false) cfg tcp ~f =
    let trace = if trace then Some (trace_out tag) else None in
    TLS.server_of_flow ?trace cfg tcp >>= function
    | Error e ->
      Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " failed"));
      Logs.warn (fun f -> f "[%s] TLS server failed %a" tag TLS.pp_write_error e);
      TCP.close tcp
    | Ok tls ->
      Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " succeeded"));
      f tls >>= fun _ ->
      TLS.close tls

  let tls_connect ~tag stack cfg addr ~f =
    TCP.create_connection (S.tcpv4 stack) addr >>= function
    | Error e ->
      Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " tcp failed"));
      Logs.warn (fun f -> f "[%s] connection failed %a" tag TCP.pp_error e);
      Lwt.return_unit
    | Ok tcp ->
      Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " tcp succeeded"));
      TLS.client_of_flow ~trace:(trace_out tag) cfg tcp >>= function
      | Error e ->
        Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " tls failed"));
        Logs.warn (fun f -> f "[%s] TLS client failed %a" tag TLS.pp_write_error e);
        TCP.close tcp
      | Ok tls ->
        Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " tls succeeded"));
        f tls >>= fun _ ->
        TLS.close tls

  let h_as_server secret cfg =
    tls_accept ~trace:true ~tag:"tls-server" cfg
      ~f:(fun tls -> TLS.write tls secret)

  let h_as_client stack secret cfg tcp =
    let ip, _ = TCP.dst tcp in
    TCP.close tcp >>= fun () ->
    tls_connect ~tag:"tls-client" stack cfg (ip, 40001)
      ~f:(fun tls -> TLS.write tls secret)

  let h_as_rev_client secret cfg tcp =
    let tag = "tls-rev-client" in
    let trace = trace_out tag in
    TLS.client_of_flow ~trace cfg tcp >>= function
    | Error e ->
      Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " failed"));
      Logs.warn (fun f -> f "TLS rev-client failed %a" TLS.pp_write_error e);
      TCP.close tcp
    | Ok tls ->
      Metrics.add pinata_stats (fun x -> x) (fun d -> d (tag ^ " succeeded"));
      TLS.write tls secret >>= fun _ ->
      TLS.close tls

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
    Metrics.add pinata_stats (fun x -> x) (fun d -> d "web");
    let len = Cstruct.len data in
    (TCP.writev tcp [ header len; data ] >|= function
      | Error e -> Logs.warn (fun f -> f "tcp error %a" TCP.pp_write_error e)
      | Ok () -> ()) >>= fun () ->
    TCP.close tcp

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

  let tls_init () =
    let now = Ptime.v (CLOCK.now_d_ps ()) in
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

  module Monitoring = Monitoring_experiments.Make(T)(Management)
  module Syslog = Logs_syslog_mirage.Udp(C)(CLOCK)(Management)

  let start c _time _mclock _pclock stack management _ info =
    let hostname = Key_gen.name ()
    and syslog = Key_gen.syslog ()
    and monitor = Key_gen.monitor ()
    in
    if Ipaddr.V4.compare syslog Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no syslog specified, dumping on stdout")
    else
      Logs.set_reporter (Syslog.create c management syslog ~hostname ());
    if Ipaddr.V4.compare monitor Ipaddr.V4.unspecified = 0 then
      Logs.warn (fun m -> m "no monitor specified, not outputting statistics")
    else
      Monitoring.create ~hostname monitor management;
    List.iter (fun (p, v) -> Logs.app (fun m -> m "used package: %s %s" p v))
      info.Mirage_info.packages;
    let ca_root, s_cfg, c_cfg, w_cfg = tls_init () in
    let secret = Cstruct.of_string (Key_gen.secret ()) in
    let web_data = Page.render ca_root in
    S.listen_tcpv4 stack ~port:80    (h_notice web_data) ;
    S.listen_tcpv4 stack ~port:443   (h_as_web_server web_data w_cfg) ;
    S.listen_tcpv4 stack ~port:10000 (h_as_server secret s_cfg) ;
    S.listen_tcpv4 stack ~port:10001 (h_as_client stack secret c_cfg) ;
    S.listen_tcpv4 stack ~port:10002 (h_as_rev_client secret c_cfg) ;
    S.listen stack
end
