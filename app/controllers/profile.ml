open Core

let conn = Db.connection

(* TODO - put this in config *)
let jwt_key = Jwt.HS256 "My secret key!!!"

module Profile = struct
  type t = {
    username: string;
    bio: string option;
    image: string option;
    following: bool;
  }

  let to_yojson { username; bio; image; following } =
    let bio_val = User.string_option_to_json bio in
    let image_val = User.string_option_to_json image in
    [%yojson
      {
        username = [%y `String username];
        bio = [%y bio_val];
        image = [%y image_val];
        following = [%y `Bool following];
      }]

  let of_user Models.User.{ username; bio; image; _ } following =
    { username; bio; image; following }
end

(* TODO - abstract this and wrap_user_json *)
let wrap_profile_json (json : Yojson.Safe.t) = `Assoc [ ("profile", json) ]

let show ?jwt_opt target =
  let%lwt profile_user_opt = Queries.Profile.show conn target in
  match profile_user_opt with
  | None -> failwith "No user with this username found!"
  | Some profile_user ->
      let%lwt following =
        match jwt_opt with
        | Some payload ->
            let source = User.get_username payload in
            let%lwt opt = Queries.Follow.show conn ~source ~target in
            Option.is_some opt |> Lwt.return
        (* TODO - check that this is intended behaviour *)
        | None -> false |> Lwt.return
      in
      let profile = Profile.of_user profile_user following in
      profile |> Profile.to_yojson |> wrap_profile_json |> Lwt.return

let follow_op query following_outcome =
  let op jwt target =
    let%lwt profile_user_opt = Queries.Profile.show conn target in
    match profile_user_opt with
    | None -> failwith "No user with this username found!"
    | Some profile_user ->
        let source = User.get_username jwt in
        let _ = query conn ~source ~target in
        let profile = Profile.of_user profile_user following_outcome in
        profile |> Profile.to_yojson |> wrap_profile_json |> Lwt.return
  in
  op

let follow = follow_op Queries.Follow.create true

let unfollow = follow_op Queries.Follow.destroy false
