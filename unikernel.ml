open Lwt
open V1
open V1_LWT

module Http = struct

  let headers status xs = 
    let headers = List.map (fun (k, v) -> k ^ ": " ^ v) xs in
    let lines   = status :: headers @ [ "\r\n" ] in
    Cstruct.of_string (String.concat "\r\n" lines)

end

module Main (C : CONSOLE) (S : STACKV4) (KV : KV_RO) (Clock : CLOCK) =
struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
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
    | `Error _ | `Eof -> log "TLS failed" ; TCP.close tcp
    | `Ok tls         -> log "TLS ok"     ; f tls >> TLS.close tls

  let tls_connect ~tag c stack cfg addr ~f =
    let log = L.log_ext tag addr in
    TCP.create_connection (S.tcpv4 stack) addr
    >>= function
    | `Error _ -> log "connection failed" ; return_unit
    | `Ok tcp  ->
        L.tracing addr @@ fun trace ->
          TLS.client_of_flow ~trace cfg "" tcp
          >>= function
          | `Error _ | `Eof -> log "TLS failed" ; TCP.close tcp
          | `Ok tls         -> log "TLS ok"     ; f tls >> TLS.close tls

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
    L.tracing peer @@ fun trace ->
      TLS.client_of_flow ~trace cfg "" tcp
      >>= function
      | `Error _ | `Eof -> log "TLS failed" ; TCP.close tcp
      | `Ok tls         -> log "TLS ok"     ; TLS.write tls secret >> TLS.close tls

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

  let valid days now =
    let asn1_of_time time =
      let tm = Clock.gmtime time in
      {
        Asn.Time.date = Clock.(tm.tm_year + 1900, (tm.tm_mon + 1), tm.tm_mday);
        time = Clock.(tm.tm_hour, tm.tm_min, tm.tm_sec, 0.);
        tz = None;
      }
    in
    let seconds = days * 24 * 60 * 60 in
    let start = asn1_of_time now in
    let expire = asn1_of_time (now +. (float_of_int seconds)) in
    (start, expire)

  let generate_ca now () =
    let rsa_key ?(bits = 2048) () =
      `RSA (Nocrypto.Rsa.generate bits)
    in
    let valid_from, valid_until = valid 365 now
    and cakey  = rsa_key ~bits:4096 ()
    and caname = [`CN "BTC Pinata CA"] in
    let cacert =
      let req        = X509.CA.request caname cakey in
      let extensions = [(true, `Basic_constraints (true, Some 1));
                        (true, `Key_usage [`Key_cert_sign])]
      in
      X509.CA.sign req ~valid_from ~valid_until ~extensions cakey caname
    in
    let gen_cert name extensions =
      let key  = rsa_key () in
      let req  = X509.CA.request [`CN name] key in
      let cert = X509.CA.sign req ~valid_from ~valid_until ~extensions cakey caname in
      ([cert; cacert], match key with `RSA k -> k)
    and extensions ?altname eku =
      (match altname with
       | None -> []
       | Some n -> [(true, `Subject_alt_name [ `DNS n ])]) @
        [(true, `Key_usage [ `Digital_signature ; `Key_encipherment ]);
         (true, `Basic_constraints (false, None)) ;
         (true, `Ext_key_usage [eku])]
    in
    (cacert,
     gen_cert "Pinata server" (extensions `Server_auth),
     gen_cert "Pinata client" (extensions `Client_auth),
     gen_cert "ownme.ipredator.se" (extensions `Server_auth ~altname:"ownme.ipredator.se"))

  let tls_init () =
    let now = Clock.time () in
    let cacert, s_cert, c_cert, w_cert = generate_ca now () in
    let authenticator = X509.Authenticator.chain_of_trust ~time:now [cacert] in
    let cacert = X509.Encoding.Pem.Certificate.to_pem_cstruct1 cacert in
    Tls.Config.(
      cacert,
      server ~certificates:(`Single w_cert) (),
      server ~authenticator ~certificates:(`Single s_cert) (),
      client ~authenticator ~certificates:(`Single c_cert) ()
    )

  let start con stack kv _ =
    L.init con stack ;
    let (ca_root, w_cfg, s_cfg, c_cfg) = tls_init () in
    lwt secret                         = KV.reads_exn kv "secret" in
    let web_data                       = Page.render ca_root in
    S.listen_tcpv4 stack ~port:80    (h_notice con web_data) ;
    S.listen_tcpv4 stack ~port:443   (h_as_web_server con stack web_data w_cfg) ;
    S.listen_tcpv4 stack ~port:10000 (h_as_server con stack secret s_cfg) ;
    S.listen_tcpv4 stack ~port:10001 (h_as_client con stack secret c_cfg) ;
    S.listen_tcpv4 stack ~port:10002 (h_as_rev_client con stack secret c_cfg) ;
    S.listen stack

end
