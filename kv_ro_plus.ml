open Lwt
open V1
open V1_LWT

module Make (KV : KV_RO) : sig
  include KV_RO
  val reads     : t -> string -> [ `Ok of Cstruct.t | `Error of error ] Lwt.t
  val reads_exn : t -> string -> Cstruct.t Lwt.t
end with type t = KV.t =
struct

  include KV

  let reads t key =
    KV.size t key >>= function
    | `Error e -> return (`Error e)
    | `Ok n    ->
        KV.read t key 0 Int64.(to_int n) >>= function
        | `Ok css  -> return (`Ok (Tls.Utils.Cs.appends css))
        | `Error e -> return (`Error e)

  let reads_exn t key =
    reads t key >>= function
    | `Error _ -> fail (Invalid_argument ("KV_RO: unknown key: " ^ key))
    | `Ok cs   -> return cs

end
