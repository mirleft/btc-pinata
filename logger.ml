open Lwt
open V1
open V1_LWT

open Sexplib

let (&.) f g x = f (g x)

module Make (C : CONSOLE) (S : STACKV4) (Clock : CLOCK) : sig
  val init : C.t ->  S.t -> unit
  val log : string -> unit
  val log_ext : string -> (Ipaddr.V4.t * int) -> string -> unit
  val tracing : (Ipaddr.V4.t * int) -> ((Sexp.t -> unit) -> unit Lwt.t) -> unit Lwt.t
end =
struct

  module TCP = Tcpv4_plus.Make (S.TCPV4)

  let log_ip      = Ipaddr.V4.of_string_exn "10.0.0.1"
  and log_port    = 12345
  and source_port = 12345

  type message =
    | Log   of string * float * string
    | Trace of (Ipaddr.V4.t * int) * float * Sexp.t

  let semaphore ?(debug=false) n =
    let pending = ref 0
    and cond    = Lwt_condition.create () in
    let rec v () =
      if !pending < n then (incr pending ; return_unit)
      else Lwt_condition.wait cond >> v ()
    and p () = decr pending ; Lwt_condition.broadcast cond ()
    and steal () = incr pending
    in
    let rec dbg () =
      if !pending > 0 then Printf.printf "* logger backlog: %d\n%!" !pending ;
      OS.Time.sleep 10. >> dbg () in
    if debug then async dbg;
    (v, p, steal)

  let (sem_v, sem_p, sem_steal) = semaphore ~debug:true 200

  let render = function
    | Log (tag, time, msg) ->
        Cstruct.of_string @@
          Printf.sprintf "%s%.04f %s\n" tag time msg
    | Trace ((host, port), time, sexp) ->
        Cstruct.of_string @@
          Printf.sprintf "[trace] %s:%d %.04f %s\n"
            (Ipaddr.V4.to_string host) port time
              (Sexp.to_string_mach sexp)

  let (chan, push) = Lwt_stream.create ()

(*   let init _ s =
    let udp = S.(udpv4 s) in
    async @@ fun () ->
      chan |> Lwt_stream.iter_s @@ fun msg ->
        S.UDPV4.write ~dest_ip:log_ip ~dest_port:log_port ~source_port udp
          (Cstruct.of_string (render msg)) *)

  let init c s =
    let t = S.(tcpv4 s) in
    let rec send cs = function
      | `Disconnected n ->
          OS.Time.sleep n >>
          TCP.create_connection t (log_ip, log_port) >>= (function
            | `Ok tcp  -> send cs (`Connected tcp)
            | `Error _ -> send cs (`Disconnected (n *. 2.)))
      | `Connected tcp ->
          TCP.write_to ~timeout:5. tcp cs >>= (function
            | `Ok () -> sem_p () ; return (`Connected tcp)
            | _      -> async (fun _ -> TCP.close tcp); send cs (`Disconnected 1.))
    in
    async @@ fun () -> Lwt_stream.fold_s (send &. render) chan (`Disconnected 0.1)

  let log msg =
    sem_steal () ;
    push (Some (Log ("", Clock.time(), msg)))

  let log_ext tag (ip, port) =
    let tag =
      Printf.sprintf "[%s] %s:%d " tag (Ipaddr.V4.to_string ip) port in
    fun msg ->
      sem_steal () ;
      push (Some (Log (tag, Clock.time(), msg)))

  let trace_period = 120.

  let atom x = Sexp.Atom x
  and list x = Sexp.List x

  module Q = struct
    let create () = ref (Some [])
    let push q x =
      match !q with
      | None    -> ()
      | Some xs -> q := Some (x :: xs)
    let close q =
      match !q with
      | None    -> None
      | Some xs -> q := None ; Some (List.rev xs)
  end

  let tracing peer f =
    let q = Q.create () in
    sem_v ()
    >>
    catch
      (fun () ->
        ( OS.Time.sleep trace_period >|= fun () ->
            Q.push q (atom "*TIMEOUT*") )
        <?>
        f (Q.push q))
      (fun exn -> return (Q.push q (atom "*ABORTED*")))
    >>
    return
    ( match Q.close q with
      | None    -> ()
      | Some xs ->
          push (Some (Trace (peer, Clock.time (), list xs))) )

end
