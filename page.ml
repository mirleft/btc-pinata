open Tyxml.Html

let btc_address = "183XuXTTgnfYfKcHbJ4sZeF46a49Fnihdh"

let header t =
  head
    (title (pcdata t))
    ([meta ~a:[a_charset "utf-8"] () ;
      style [pcdata
    {___|body {
           font-family: monospace;
           color: #333;
         }
         .content {
           margin: 10% 0 10% 15%;
           width: 45%;
         }
         .content h3 {
           font-size: 35px;
         }
         a, a:visited {
           color: #333;
           text-decoration: none;
           font-weight: bold;
         }
         #logo {
           z-index: -1;
           opacity: 0.9;
           position: fixed;
           width: 40%;
           top: 0;
           right: 0;
         }|___} ]
     ])

let link ~href child = a ~a:[a_href href] [pcdata child ]

let content ca_root =
  let a_chain = link
    ~href:("https://blockchain.info/address/" ^ btc_address)
    btc_address
  and a_mirage = link ~href:"https://mirage.io" "MirageOS"
  and a_solo5 = link ~href:"https://github.com/solo5/Solo5" "Solo5"
  and a_pinata = link ~href:"https://github.com/mirleft/btc-pinata" "BTC Piñata"
  and a_tls    = link ~href:"https://github.com/mirleft/ocaml-tls" "TLS"
  and a_x509   = link ~href:"https://github.com/mirleft/ocaml-x509" "X.509"
  and a_path_val = link
    ~href:"https://tools.ietf.org/html/rfc5280#page-71"
    "path validation"
  and a_ipredator = link ~href:"https://www.ipredator.se" "IPredator"
  and a_full_list = link ~href:"https://raw.githubusercontent.com/mirleft/btc-pinata/master/opam-full.txt" "full list"
  and a_unikernel = link ~href:"https://raw.githubusercontent.com/mirleft/btc-pinata/master/pinata.ukvm.xz" "toy unikernel"
  and a_31c3 = link ~href:"http://media.ccc.de/browse/congress/2014/31c3_-_6443_-_en_-_saal_2_-_201412271245_-_trustworthy_secure_modular_operating_system_engineering_-_hannes_-_david_kaloper.html#video" "31c3 talk"
  and a_schneier = link ~href:"https://www.schneier.com/crypto-gram/archives/1998/1215.html#contests" "bounties"
  and a_https = link ~href:"https://ownme.ipredator.se" "HTTPS"
  and a_recap = link ~href:"https://mirage.io/blog/bitcoin-pinata-results" "first five months of the Piñata"
  and a_nqsb = link ~href:"https://nqsb.io" "not quite so broken"
  and a_not = link ~href:"https://nqsb.io" "https://nqsb.io"
  in
  let ca = pre [ pcdata (Cstruct.to_string ca_root) ]
  in

  div ~a:[a_class ["content"]] [
    h3 [ pcdata "You have reached the BTC Piñata." ] ;
    br () ;
    p [ pcdata "BTC Piñata knows the private key to the bitcoin address " ; a_chain ; pcdata ". If you break the Piñata, you get to keep what's inside." ] ;
    p [ pcdata "Here are the rules of the game:" ] ;
    ul [
      li [ p [ pcdata "You can connect to port 10000 using TLS. Piñata will send the key and hang up." ] ] ;
      li [ p [ pcdata "You can connect to port 10001 using TCP. Piñata will immediately close the connection and connect back over TLS to port 40001 on the initiating host, send the key, and hang up." ] ] ;
      li [ p [ pcdata "You can connect to port 10002 using TCP. Piñata will initiate a TLS handshake over that channel serving as a client, send the key over TLS, and hang up." ] ]
    ] ;
    p [ pcdata "And here's the kicker: in both the client and server roles, Piñata requires the other end to present a certificate. Authentication is performed using standard " ; a_path_val ; pcdata " with a single certificate as the trust anchor. And no, you can't have the certificate key." ] ;
    p [ pcdata "It follows that it should be impossible to successfully establish a TLS connection as long as Piñata is working properly. To get the spoils, you have to smash it." ] ;
    p [ pcdata "Before you ask: yes, Piñata will talk to itself and you can enjoy watching it do so." ] ;
    br () ;
    p [ a_pinata ; pcdata " is a " ; a_mirage ; pcdata " unikernel using " ; a_nqsb ; pcdata " software. It is written in OCaml, runs directly on FreeBSD VMM (using " ; a_solo5 ; pcdata "), and is using native OCaml " ; a_tls ; pcdata " and " ; a_x509 ; pcdata " implementations." ] ;
    p [ pcdata "The " ; a_full_list ; pcdata " of installed software and a " ; a_unikernel ; pcdata " without secrets are available. There is no need to use the old automated tools on Piñata - roll your own instead. This challenge started in February 2015, and will run until the above address no longer contains the 10 bitcoins it started with, or until we lose interest. In 2018 we will likely reuse most bitcoins for other projects." ] ;
    p [ pcdata "Why are we doing this? At the beginning of 2014 we started to develop a " ; a_nqsb ; pcdata " TLS implementation from scratch. You can read more about it on " ; a_not ; pcdata " or watch our " ; a_31c3 ; pcdata " about it. We want to boost our confidence in the TLS implementation we've developed and show that robust systems software can be written in a functional language. We recapitulated the " ; a_recap ; pcdata "." ] ;
    p [ pcdata "We are well aware that " ; a_schneier ; pcdata " can only disprove the security of a system, and never prove it. We won't take home the message that we are 'unbreakable', 'correct', and especially not 'secure'. But we don't rely on obscurity and have a fully transparent implementation of a well-known protocol. Our prize is publicly observable in the blockchain.  If you observe a transaction, it is taken. So if this contest attracts attention and we are still standing at the end of it, we will gain that extra inch of confidence in our work." ] ;
    p [ pcdata "This page is also available via " ; a_https ; pcdata ". It will present a certificate signed by the same authority that Piñata expects to sign all of the incoming requests, so your browser will complain. The purpose of HTTPS is to allow checking of interoperability with our TLS implementation." ] ;
    br () ;
    p [ pcdata "Bitcoins and the hosting for this challenge are sponsored by " ; a_ipredator ; pcdata ", a friendly virtual private network provider!" ] ;
    p [ pcdata "If you have any results or further questions, don't hesitate to contact us. Address is anything at nqsb dot io." ] ;
    br () ;
    p [ pcdata "This is the CA:" ] ;
    ca
  ]

let logo =
{___|<svg id="logo" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://www.w3.org/2000/svg" enable-background="new 0 0 364 364" xml:space="preserve" viewBox="0 0 364 364" version="1.1" y="0px" x="0px" xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/">
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
    </svg>|___}

let render ca_root =
  let buf = Buffer.create 500 in
  let fmt = Format.formatter_of_buffer buf in
  pp () fmt @@
  html
    (header "BTC Piñata")
    (body [Unsafe.data logo ; content ca_root]) ;
  Cstruct.of_string @@ Buffer.contents buf
