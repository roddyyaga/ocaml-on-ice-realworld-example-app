open Core
open Models.User

let all_query =
  Caqti_request.collect Caqti_type.unit
    Caqti_type.(
      let ( & ) = tup2 in
      int & string & string & string & option string & option string)
    {sql| SELECT id, username, email, password_hash, bio, image FROM users |sql}

let all (module Db : Caqti_lwt.CONNECTION) =
  let result =
    Db.fold all_query
      (fun (id, (username, (email, (password_hash, (bio, image))))) acc ->
        { id; username; email; password_hash; bio; image } :: acc)
      () []
  in
  Ocoi.Db.handle_caqti_result result

let show_query =
  Caqti_request.find_opt Caqti_type.int
    Caqti_type.(
      let ( & ) = tup2 in
      int & string & string & string & option string & option string)
    {sql| SELECT id, username, email, password_hash, bio, image
          FROM users
          WHERE id = (?)
    |sql}

let get_one_param query =
  let inner (module Db : Caqti_lwt.CONNECTION) arg =
    let result = Db.find_opt query arg in
    let%lwt data = Ocoi.Db.handle_caqti_result result in
    let record =
      Option.map
        ~f:(fun (id, (username, (email, (password_hash, (bio, image))))) ->
          { id; username; email; password_hash; bio; image })
        data
    in
    Lwt.return record
  in
  inner

let show = get_one_param show_query

let get_by_email_query =
  Caqti_request.find_opt Caqti_type.string
    Caqti_type.(
      let ( & ) = tup2 in
      int & string & string & string & option string & option string)
    {sql| SELECT id, username, email, password_hash, bio, image
          FROM users
          WHERE email = (?)
    |sql}

let get_by_email = get_one_param get_by_email_query

let get_by_username_query =
  Caqti_request.find_opt Caqti_type.string
    Caqti_type.(
      let ( & ) = tup2 in
      int & string & string & string & option string & option string)
    {sql| SELECT id, username, email, password_hash, bio, image
          FROM users
          WHERE username = (?)
    |sql}

let get_by_username = get_one_param get_by_username_query

let create_query =
  Caqti_request.find
    Caqti_type.(
      let ( & ) = tup2 in
      string & string & string & option string & option string)
    Caqti_type.int
    {sql| INSERT INTO users (username, email, password_hash, bio, image)
          VALUES (?, ?, ?, ?, ?) RETURNING id |sql}

let create (module Db : Caqti_lwt.CONNECTION) ~username ~email ~password_hash
    ~bio ~image =
  let result =
    Db.find create_query (username, (email, (password_hash, (bio, image))))
  in
  Ocoi.Db.handle_caqti_result result

let update_query =
  Caqti_request.exec
    Caqti_type.(
      let ( & ) = tup2 in
      string & string & string & option string & option string & int)
    {sql| UPDATE users
       SET (username, email, password_hash, bio, image) = (?, ?, ?, ?, ?)
       WHERE id = (?)
    |sql}

let update (module Db : Caqti_lwt.CONNECTION)
    { id; username; email; password_hash; bio; image } =
  let result =
    Db.exec update_query (username, (email, (password_hash, (bio, (image, id)))))
  in
  Ocoi.Db.handle_caqti_result result

let destroy_query =
  Caqti_request.exec Caqti_type.int {sql| DELETE FROM users WHERE id = (?) |sql}

let destroy (module Db : Caqti_lwt.CONNECTION) id =
  let result = Db.exec destroy_query id in
  Ocoi.Db.handle_caqti_result result

let migrate_query =
  Caqti_request.exec Caqti_type.unit
    {sql| CREATE TABLE users (
         id SERIAL PRIMARY KEY NOT NULL,
         username VARCHAR NOT NULL UNIQUE,
         email VARCHAR NOT NULL UNIQUE,
         password_hash VARCHAR NOT NULL,
         bio VARCHAR,
         image VARCHAR
       )
    |sql}

let migrate (module Db : Caqti_lwt.CONNECTION) = Db.exec migrate_query ()

let rollback_query =
  Caqti_request.exec Caqti_type.unit {sql| DROP TABLE users |sql}

let rollback (module Db : Caqti_lwt.CONNECTION) = Db.exec rollback_query ()
