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
          }
          #content {
            margin: 10% 0 10% 15%;
            width: 45%;
          }
          h3 { text-shadow: 1px 1px #ccc; }
          a, a:visited { color: #333; }
          #logo {
            z-index: -1;
            opacity: 0.9;
            position: fixed;
            width: 40%;
            top: 0;
            right: 0;
          }
        </style>
      </head>
      <body>$body$</body>
    </html>
  >>

let link ~href child = [ Html.a ~href:(Uri.of_string href) child ]

let content ca_root =
  let a_chain = link
    ~href:("https://blockchain.info/address/" ^ btc_address)
    <:html<$Html.html_of_string btc_address$>>
  and a_mirage = link ~href:"http://openmirage.org" <:html<Mirage>>
  and a_pinata = link ~href:"https://github.com/mirleft/btc-pinata" <:html<BTC Piñata>>
  and a_tls    = link ~href:"https://github.com/mirleft/ocaml-tls" <:html<TLS>>
  and a_x509   = link ~href:"https://github.com/mirleft/ocaml-x509" <:html<X.509>>
  and a_path_val = link
    ~href:"https://tools.ietf.org/html/rfc5280#page-71"
    <:html<path validation>>
  and a_ipredator = link ~href:"https://www.ipredator.se" <:html<IPredator>>
  and a_full_list = link ~href:"https://raw.githubusercontent.com/mirleft/btc-pinata/master/opam-full.txt" <:html<full list>>
  and a_unikernel = link ~href:"https://raw.githubusercontent.com/mirleft/btc-pinata/master/btc-pinata.xen" <:html<unikernel>>
  in
  let ca = <:html<
    <pre>$Html.html_of_string (Cstruct.to_string ca_root)$</pre>
  >>
  in
  <:html<

    <svg id="logo" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://www.w3.org/2000/svg" enable-background="new 0 0 364 364" xml:space="preserve" viewBox="0 0 364 364" version="1.1" y="0px" x="0px" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/">
      <g stroke="#000" stroke-miterlimit="10" fill="#fff">
        <path stroke-linejoin="round" d="m125.29 212.04-75.584 7.664 66.914-37.523c-0.599-7.652-0.062-15.523 1.758-23.398 3.68-15.921 12.059-29.525 23.316-39.723l-17.053-56.102 44.406 40.066-0.018 0.012c8.447-2.836 17.454-4.229 26.662-3.955l32.709-58.913-2.154 66.195c14.354 6.723 25.964 17.595 33.701 30.774l-0.046-0.116 54.511-5.706-45.61 28.354-0.012-0.028c2.381 10.829 2.462 22.341-0.202 33.862-3.092 13.379-9.508 25.117-18.154 34.58 0.02 0.03 0.031 0.051 0.031 0.051l22.76 72.487-51.25-51.361s-0.339-0.435-0.699-1.213c-9.997 3.875-20.879 5.68-32.012 5.056l-26.145 54.399 0.379-60.355c-16.772-7.06-30.074-19.671-38.208-35.107z" stroke-width="40"/>
        <g stroke-dasharray="4">
          <polygon transform="matrix(-1 0 0 1 364.13 0)" points="201 307.5 200.57 238.61 171.16 245.41"/>
          <polygon transform="matrix(-1 0 0 1 364.13 0)" points="195.07 103.03 222.24 119.66 239.48 62.961"/>
          <polygon transform="matrix(-1 0 0 1 364.13 0)" points="49.709 131.32 104.22 137.02 95.319 159.67"/>
          <circle cx="-193.49" transform="scale(-1,1)" cy="176.15" r="77.086"/>
          <path d="m250.47 228.14 22.76 72.487-51.25-51.361s-6.998-8.949 8.889-18.473c15.885-9.523 19.601-2.653 19.601-2.653z"/>
          <path d="m135.87 210.97-86.162 8.737 75.297-42.225s12.918-4.169 18.271 10.411c5.354 14.582-7.406 23.077-7.406 23.077z"/>
          <path d="m225.53 128.45 2.873-88.275-43.08 77.595s-6.673 14.228 17.098 19.722c23.771 5.493 23.109-9.042 23.109-9.042z"/>
        </g>
        <g stroke-linejoin="round" stroke-linecap="round" stroke-width="23">
          <line y2="275" x2="49.709" y1="230.33" x1="49.709"/>
          <line y2="306.33" x2="74.625" y1="230.33" x1="74.625"/>
          <line y2="253.25" x2="99.291" y1="230.33" x1="99.291"/>
        </g>
      </g>
    </svg>

    <div id="content">

      <h3>You have reached the BTC Piñata</h3>

      <p>BTC Piñata knows the private key to the bitcoin address $a_chain$. If
      you break the piñata, you get to keep what's inside.</p>

      <p>Here are the rules of the game:</p>

      <ul>
        <li>
          <p>You can connect to the port 10000 using TLS. Piñata will send the
          key and hang up.</p>
        </li>
        <li>
          <p>You can connect to the port 10001 using TCP. Piñata will immediately
          close the connection and connect back over TLS to port 40001 on the
          initiating host, send the key, and hang up.</p>
        </li>
        <li>
          <p>You can connect to the port 10002 using TCP. Piñata will initiate a
          TLS handshake over that channel serving as a client, send the key over
          TLS, and hang up.</p>
        </li>
      </ul>

      <p>And here's the kicker: in both the client and server roles, piñata
      requires the other end to present a certificate. Authentication is performed
      using standard $a_path_val$ with a single certificate as the trust
      anchor. And no, you can't have its key.</p>

      <p>It follows that it should be impossible to successfully establish a TLS
      connection as long as piñata is working properly. To get the spoils, you
      have to smash it.</p>

      <p>Before you ask: yes, piñata will talk to itself and you can enjoy
      watching it do so.</p>

      <br/>

      <p>$a_pinata$ is a $a_mirage$ unikernel. It was written in OCaml, runs
      directly on Xen, and is using native OCaml $a_tls$ and $a_x509$
      implementations.</p>

      <p>The $a_full_list$ of installed software and a toy $a_unikernel$ (no
      secrets included) are available. There is no need to use your automated
      tools on piñata - roll your own instead. This challenge runs until mid
      March 2015, or until the above address no longer contains the 10 Bitcoins
      it started with.</p>

      <p>Bitcoins and the hosting for this challenge are sponsored by
      $a_ipredator$, a friendly virtual private network provider!</p>

      <br/>

      <p>This is the CA:</p>
      $ca$

      <p>If you have any results or further questions, don't hesitate to contact
      us. Address is in the certificate.</p>

    </div>

  >>

let render ca_root =
  Cstruct.of_string @@ Html.to_string @@
    wrap_body ~title:"BTC Piñata" ~body:(content ca_root)
