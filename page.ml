open Cow

let btc_address = "183XuXTTgnfYfKcHbJ4sZeF46a49Fnihdh"

let wrap_body ~title ~body =
  <:html<
    <html>
      <head>
        <title>$Html.html_of_string title$</title>
        <style>
          body {
            font-family: sans-serif;
            color: #333;
            margin: 5%;
          }
          a { color: #333; }
          a:visited { color: #333; }
        </style>
      </head>
      <body>$body$</body>
    </html>
  >>

let link ~href child = [ Html.link ~href:(Uri.of_string href) child ]

let content ca_root =
  let a_chain = link
    ~href:("https://blockchain.info/address/" ^ btc_address)
    <:html< <tt>$Html.html_of_string btc_address$</tt> >>
  and a_mirage = link ~href:"http://openmirage.org" <:html<Mirage>>
  and a_pinata = link ~href:"https://github.com/mirleft/btc-piñata" <:html<BTC Piñata>>
  and a_tls    = link ~href:"https://github.com/mirleft/ocaml-tls" <:html<TLS>>
  and a_x509   = link ~href:"https://github.com/mirleft/ocaml-x509" <:html<X.509>>
  and a_path_val = link
    ~href:"https://tools.ietf.org/html/rfc5280#page-71"
    <:html<path validation>> in
  let ca = Html.html_of_string (Cstruct.to_string ca_root) in
  let ca = <:html< <pre>$ca$</pre> >>
  in
  <:html<

    <p><b>You have reached the BTC Piñata.</b></p>

    <p>BTC Piñata knows the private key to the bitcoin address $a_chain$.
    If you break the piñata, you get to keep what's inside.</p>

    <p>Here are the rules of the game:</p>

    <ul>
      <li>
        <p>You can connect to the port 30001 using TLS. Piñata will send the
        key and hang up.</p>
      </li>
      <li>
        <p>You can connect to the port 30002 using TCP. Piñata will immediately
        close the connection and connect back over TLS to port 40001 on the
        initiating host, send the key, and hang up.</p>
      </li>
      <li>
        <p>You can connect to the port 30003 using TCP. Piñata will initiate a
        TLS handshake over that channel serving as a client, send the key over
        TLS, and hang up.</p>
      </li>
    </ul>

    <p>And here's the kicker: in both client and server role, piñata requires
    the other end to present a certificate. Authentication is performed using
    standard $a_path_val$, but allowing only one certificate as the trust
    anchor. And no, you can't have its key.</p>

    <p>It follows that it should be impossible to successfully establish a
    TLS connection as long as piñata is working properly: to get the spoils, you
    have to smash it.</p>

    <p>$a_pinata$ is a $a_mirage$ unikernel. It is written in OCaml, runs
    directly on Xen, and is using native OCaml $a_tls$ and $a_x509$
    implementations.</p>

    <p>This is the CA:</p>
    $ca$
  >>

let render ca_root =
  Cstruct.of_string @@ Html.to_string @@
    wrap_body ~title:"BTC Piñata" ~body:(content ca_root)


