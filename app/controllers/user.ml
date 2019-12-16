open Core

let conn = Db.connection

(* TODO - put this in config *)
let jwt_key = Jwt.HS256 "My secret key!!!"

let make_token identity =
  let open Ocoi.Jwt_utils in
  make_and_encode ~algorithm:jwt_key
    [ ("iss", "OCaml RealWorld App"); ("sub", identity) ]

module Crud : Ocoi.Controllers.Crud = struct
  include Models.User

  let create json =
    let open Yojson.Safe.Util in
    let json = json |> member "user" in
    let username = json |> member "username" |> to_string in
    let email = json |> member "email" |> to_string in
    let password = json |> member "password" |> to_string in
    let password_hash = Bcrypt.(password |> hash |> string_of_hash) in
    let bio = json |> member "bio" |> to_string_option in
    let image = json |> member "image" |> to_string_option in
    Queries.User.create conn ~username ~email ~password_hash ~bio ~image

  let index () = Queries.User.all conn

  let show id = Queries.User.show conn id

  let update { id; username; email; password_hash; bio; image } =
    Queries.User.update conn { id; username; email; password_hash; bio; image }

  let destroy id = Queries.User.destroy conn id
end

module User_no_password = struct
  type t = {
    username: string;
    email: string;
    bio: string option;
    image: string option;
  }
  [@@deriving yojson]

  let remove_password Models.User.{ username; email; bio; image; _ } =
    { username; email; bio; image }
end

(* TODO - put somewhere more sensible since also used by profiles *)
let string_option_to_json string_opt =
  match string_opt with Some s -> `String s | None -> `Null

(* TODO - override to_yojson? *)
let make_user_json Models.User.{ username; email; bio; image; _ } =
  let token = make_token username in
  let bio_val = string_option_to_json bio in
  let image_val = string_option_to_json image in
  [%yojson
    {
      username = [%y `String username];
      email = [%y `String email];
      token = [%y `String token];
      bio = [%y bio_val];
      image = [%y image_val];
    }]

let wrap_user_json (json : Yojson.Safe.t) = `Assoc [ ("user", json) ]

let create json =
  let open Yojson.Safe.Util in
  let json = json |> member "user" in
  let username = json |> member "username" |> to_string in
  let email = json |> member "email" |> to_string in
  let password = json |> member "password" |> to_string in
  let password_hash = Bcrypt.(password |> hash |> string_of_hash) in
  let bio = json |> member "bio" |> to_string_option in
  let image = json |> member "image" |> to_string_option in
  let%lwt id =
    Queries.User.create conn ~username ~email ~password_hash ~bio ~image
  in
  let user = Models.User.{ id; username; email; bio; image; password_hash } in
  user |> make_user_json |> wrap_user_json |> Lwt.return

let login json =
  let open Yojson.Safe.Util in
  let json = json |> member "user" in
  let email = json |> member "email" |> to_string in
  let supplied_password = json |> member "password" |> to_string in
  let%lwt user_opt = Queries.User.get_by_email conn email in
  match user_opt with
  | None -> failwith "User doesn't exist!"
  | Some user -> (
      let stored_hash = Bcrypt.hash_of_string user.password_hash in
      let correct = Bcrypt.verify supplied_password stored_hash in
      match correct with
      | false -> failwith "Invalid password!"
      | true -> user |> make_user_json |> wrap_user_json |> Lwt.return )

let get_username jwt =
  let open Yojson.Basic.Util in
  match jwt |> Jwt.json_of_payload |> member "sub" with
  | `String username -> username
  | _ ->
      let to_str = Jwt.string_of_payload jwt in
      failwith
        (Printf.sprintf
           "Unexpected JWT data, was expecting [(\"sub\", username)] but got %s"
           to_str)

let show jwt =
  let username = get_username jwt in
  let%lwt user_opt = Queries.User.get_by_username conn username in
  match user_opt with
  | None -> failwith "User doesn't exist!"
  | Some user -> user |> make_user_json |> wrap_user_json |> Lwt.return

(** Takes a JSON representation of an old user (with all fields) and one of a new user (possibly missing fields) and
 * returns a user record. *)
let update_user_jsons old_user new_user =
  let open Yojson.Safe.Util in
  let old_assoc, new_assoc = (old_user |> to_assoc, new_user |> to_assoc) in
  let get = List.Assoc.find ~equal:( = ) in
  let f (k, v) =
    match k with
    | "password_hash" -> (
        let new_password_opt = get new_assoc "password" in
        match new_password_opt with
        | Some (`String new_password) ->
            let new_password_hash =
              Bcrypt.(new_password |> hash |> string_of_hash)
            in
            (k, `String new_password_hash)
        | _ -> (k, v) )
    | _ -> (k, Option.value ~default:v (get new_assoc k))
  in
  (*(k, Option.value (List.Assoc.find ~equal:(=) new_assoc k) ~default:v) in*)
  match `Assoc (List.map ~f old_assoc) |> Models.User.of_yojson with
  | Ok user -> user
  | Error _ -> failwith "Unexpected error in creating user from JSON"

let update jwt json =
  let username = get_username jwt in
  let%lwt user_opt = Queries.User.get_by_username conn username in
  match user_opt with
  | None -> failwith "User doesn't exist!"
  | Some old_user ->
      let open Yojson.Safe.Util in
      let fields_to_update = json |> member "user" in
      let new_user =
        update_user_jsons (Models.User.to_yojson old_user) fields_to_update
      in
      let%lwt () = Queries.User.update conn new_user in
      new_user |> make_user_json |> wrap_user_json |> Lwt.return
