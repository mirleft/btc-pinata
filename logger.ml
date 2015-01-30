open Lwt
open V1
open V1_LWT

module Make (C : CONSOLE) (S : STACKV4) (Clock : CLOCK) : sig
  val init : C.t ->  S.t -> unit
  val log : string -> unit
  val log_ext : string -> (Ipaddr.V4.t * int) -> string -> unit
  val trace : (Ipaddr.V4.t * int) -> Sexplib.Sexp.t -> unit
end =
struct

  module TCP = Tcpv4_plus.Make (S.TCPV4)

  let log_ip      = Ipaddr.V4.of_string_exn "10.0.0.1"
  and log_port    = 12345
  and source_port = 12345


  type message =
      Log   of string * float * string
    | Trace of string * (Ipaddr.V4.t * int) * int * float * Sexplib.Sexp.t

  let render = function
    | Log (tag, time, msg) -> Printf.sprintf "%s%.04f %s\n" tag time msg
    | Trace (id, (host, port), seq, time, sexp) ->
        Printf.sprintf "[trace] %s %s:%d %d %.04f %s\n"
          id (Ipaddr.V4.to_string host) port seq time
          (Sexplib.Sexp.to_string sexp)

  let (chan, push) = Lwt_stream.create ()

(*   let init _ s =
    let udp = S.(udpv4 s) in
    async @@ fun () ->
      chan |> Lwt_stream.iter_s @@ fun msg ->
        S.UDPV4.write ~dest_ip:log_ip ~dest_port:log_port ~source_port udp
          (Cstruct.of_string (render msg)) *)

  let init c s =
    let t = S.(tcpv4 s) in
    let rec send msg = function
      | `Disconnected n ->
          OS.Time.sleep n >>
          TCP.create_connection t (log_ip, log_port) >>= (function
            | `Ok tcp  -> send msg (`Connected tcp)
            | `Error _ -> send msg (`Disconnected (n *. 2.)))
      | `Connected tcp ->
          let cs = Cstruct.of_string (render msg) in
          TCP.write_to ~timeout:5. tcp cs >>= (function
            | `Ok () -> return (`Connected tcp)
            | _      -> async (fun _ -> TCP.close tcp); send msg (`Disconnected 1.))
    in
    async @@ fun () -> Lwt_stream.fold_s send chan (`Disconnected 0.1)

  let log msg =
    push (Some (Log ("", Clock.time(), msg)))

  let log_ext tag (ip, port) =
    let tag =
      Printf.sprintf "[%s] %s:%d " tag (Ipaddr.V4.to_string ip) port in
    fun msg -> push (Some (Log (tag, Clock.time(), msg)))

  let string_of_cs_hex cs =
    let buf = Buffer.create 32 in
    let emit x =
      Buffer.add_char buf (Char.chr (x + if x < 10 then 48 else 87)) in
    for i = 0 to Cstruct.len cs - 1 do
      let n = Cstruct.get_uint8 cs i in
      emit (n land 0x0f) ; emit ((n land 0xf0) lsr 4)
    done ;
    Buffer.contents buf

  let trace peer =
    let id = string_of_cs_hex (Nocrypto.Rng.generate 8)
    and n   = ref 0 in
    fun sexp ->
      incr n ;
      push (Some (Trace (id, peer, !n, Clock.time(), sexp)))

end
