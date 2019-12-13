open Core
open Opium.Std
open Controllers

let register_jwt_only verb path f ~algorithm =
  verb path (fun req ->
      let token = Utils.get_token req in
      match token with
      | Some token -> (
          let decoded = Ocoi.Jwt_utils.verify_and_decode ~algorithm token in
          match decoded with
          | Ocoi.Jwt_utils.Payload payload ->
              let%lwt response_content = f payload in
              `Json response_content |> respond'
          | Ocoi.Jwt_utils.SignatureMismatch | Ocoi.Jwt_utils.FormatError ->
              Ocoi.Handler_utils.empty_response `Unauthorized )
      | None -> Ocoi.Handler_utils.empty_response `Unauthorized)

let register_jwt_json verb path f ~algorithm =
  verb path (fun req ->
      let token = Utils.get_token req in
      match token with
      | Some token -> (
          let decoded = Ocoi.Jwt_utils.verify_and_decode ~algorithm token in
          match decoded with
          | Ocoi.Jwt_utils.Payload payload ->
              let%lwt json = App.json_of_body_exn req in
              let%lwt response_content = f payload json in
              `Json response_content |> respond'
          | Ocoi.Jwt_utils.SignatureMismatch | Ocoi.Jwt_utils.FormatError ->
              Ocoi.Handler_utils.empty_response `Unauthorized )
      | None -> Ocoi.Handler_utils.empty_response `Unauthorized)

let hello_world =
  get "/" (fun _ -> `String "Hello world!\n\nfrom OCaml\n     Ice" |> respond')

let create_user = Ocoi.Controllers.register_json post "/users" User.create

let login = Ocoi.Controllers.register_json post "/users/login" User.login

let show_user = register_jwt_only get "/user" User.show ~algorithm:User.jwt_key

let update_user =
  register_jwt_json put "/user" User.update ~algorithm:User.jwt_key

let _ =
  let app = Ocoi.App.base in
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Info);
  let app =
    app
    |> Ocoi.Controllers.register_crud "/users2" (module User.Crud)
    |> create_user |> login |> show_user |> update_user
  in
  app |> App.run_command
