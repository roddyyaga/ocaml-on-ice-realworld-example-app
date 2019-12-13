open Core
open Opium.Std

(** JWT token given with Token as type rather than Bearer so we need custom decoder. *)
let get_token req =
  let auth_value = req |> Request.headers |> Ocoi.Auth.get_authorization in
  match auth_value with
  | Some (`Other s) -> (
      match String.lsplit2 ~on:' ' s with
      | Some ("Token", t) -> Some t
      | _ -> None )
  | _ -> None
