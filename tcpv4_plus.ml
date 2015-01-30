open Lwt
open V1
open V1_LWT

module Make (TCP : TCPV4) : sig
  include TCPV4
  val write_to : timeout:float -> flow -> Cstruct.t -> [ `Ok of unit | `Error of error | `Eof | `Timeout ] Lwt.t
end
with type t     = TCP.t
and  type error = TCP.error
and  type flow  = TCP.flow
= struct

  include TCP

  let write_to ~timeout flow cs =
    let write = TCP.write flow cs >|= function
      | `Ok () | `Error _ | `Eof as r -> r in
    pick [ write ; OS.Time.sleep timeout >> return `Timeout ]

end
