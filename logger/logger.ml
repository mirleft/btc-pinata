open Lwt

let string_of_sockaddr = function
  | Unix.ADDR_UNIX addr -> addr
  | Unix.ADDR_INET (addr, port) ->
      Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let listen ~addr:(host, port) =
  let open Lwt_unix in
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;
  bind s (ADDR_INET (host, port));
  listen s 10;
  s

let accept socket =
  Lwt_unix.accept socket >|= fun (s, peer) ->
    (Lwt_io.(of_fd ~mode:input s, of_fd ~mode:output s), peer)

let logger ~file =
  let (stream, push) = Lwt_stream.create () in
  let write msg =
    try_lwt
      lwt ch =
        Lwt_io.open_file
          ~flags:Lwt_unix.([O_WRONLY; O_APPEND; O_CREAT])
          ~mode:Lwt_io.output
          file in
      Lwt_io.write_line ch msg >> Lwt_io.close ch
    with exn ->
      Lwt_io.printf "ERROR writing message: %s\n%!" msg in
  async (fun () -> Lwt_stream.iter_s write stream);
  return (fun msg -> push (Some msg))

let rec forever act = act () >> forever act

let main ~addr ~file =
  let server = listen ~addr in
  lwt sink   = logger ~file in
  forever @@ fun () ->
    lwt ((ic, _), peer) = accept server in
    Lwt_io.printf "[accept] %s\n%!" (string_of_sockaddr peer)
    >|= fun () ->
    async (fun () ->
      Lwt_stream.iter sink (Lwt_io.read_lines ic))

let () =
  let (port, file) =
    Sys.(int_of_string argv.(1), argv.(2)) in
  Lwt_main.run (main ~addr:(Unix.inet_addr_any, port) ~file)

