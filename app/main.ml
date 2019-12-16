open Core
open Opium.Std
open Controllers

let register_jwt verb path f ~algorithm =
  verb path (fun req ->
      let token = Utils.get_token req in
      match token with
      | Some token -> (
          let decoded = Ocoi.Jwt_utils.verify_and_decode ~algorithm token in
          match decoded with
          | Ocoi.Jwt_utils.Payload payload ->
              let%lwt response_content = f req payload in
              `Json response_content |> respond'
          | Ocoi.Jwt_utils.SignatureMismatch | Ocoi.Jwt_utils.FormatError ->
              Ocoi.Handler_utils.empty_response `Unauthorized )
      | None -> Ocoi.Handler_utils.empty_response `Unauthorized)

let register_jwt_only verb path f ~algorithm =
  let inner _ payload = f payload in
  register_jwt verb path ~algorithm inner

let register_jwt_json verb path f ~algorithm =
  let inner req payload =
    let%lwt json = App.json_of_body_exn req in
    f payload json
  in
  register_jwt verb path ~algorithm inner

let hello_world =
  get "/" (fun _ -> `String "Hello world!\n\nfrom OCaml\n     Ice" |> respond')

let create_user = Ocoi.Controllers.register_json post "/users" User.create

let login = Ocoi.Controllers.register_json post "/users/login" User.login

let show_user = register_jwt_only get "/user" User.show ~algorithm:User.jwt_key

let update_user =
  register_jwt_json put "/user" User.update ~algorithm:User.jwt_key

let show_profile =
  get "/profiles/:username" (fun req ->
      let token = Utils.get_token req in
      let jwt_opt_result =
        match token with
        | Some token -> (
            let algorithm = User.jwt_key in
            let decoded = Ocoi.Jwt_utils.verify_and_decode ~algorithm token in
            match decoded with
            | Ocoi.Jwt_utils.Payload payload -> Ok (Some payload)
            | other -> Error other )
        | None -> Ok None
      in
      match jwt_opt_result with
      | Ok jwt_opt ->
          let%lwt profile_json = Profile.show ?jwt_opt (param req "username") in
          `Json profile_json |> respond'
      | Error _ -> Ocoi.Handler_utils.empty_response `Unauthorized)

let follow =
  let inner req payload =
    let target = param req "username" in
    Profile.follow payload target
  in
  register_jwt post "/profiles/:username/follow" ~algorithm:User.jwt_key inner

let unfollow =
  let inner req payload =
    let target = param req "username" in
    Profile.unfollow payload target
  in
  register_jwt delete "/profiles/:username/follow" ~algorithm:User.jwt_key inner

let _ =
  let app = Ocoi.App.base in
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Info);
  let app =
    app
    |> Ocoi.Controllers.register_crud "/users2" (module User.Crud)
    |> create_user |> login |> show_user |> update_user |> show_profile
    |> follow |> unfollow
  in
  app |> App.run_command
