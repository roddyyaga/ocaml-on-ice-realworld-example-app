let show_query =
  Caqti_request.find_opt
    Caqti_type.(tup2 string string)
    Caqti_type.(tup2 int int)
    {sql| SELECT sources.id, targets.id
          FROM users sources, users targets, follows
          WHERE sources.username = (?) AND targets.username = (?)
            AND sources.id = follows.source_id AND targets.id = follows.target_id
    |sql}

let show (module Db : Caqti_lwt.CONNECTION) ~source ~target =
  let result = Db.find_opt show_query (source, target) in
  let%lwt data = Ocoi.Db.handle_caqti_result result in
  Lwt.return data

let create_query =
  Caqti_request.exec
    Caqti_type.(tup2 string string)
    {sql|
    INSERT INTO follows (source_id, target_id)
    (SELECT sources.id, targets.id FROM users sources, users targets
    WHERE sources.username = (?) AND targets.username = (?))
    ON CONFLICT DO NOTHING
    |sql}

let create (module Db : Caqti_lwt.CONNECTION) ~source ~target =
  let result = Db.exec create_query (source, target) in
  Ocoi.Db.handle_caqti_result result

let destroy_query =
  Caqti_request.exec
    Caqti_type.(tup2 string string)
    {sql|
    DELETE FROM follows
    USING users sources, users targets
    WHERE sources.username = (?) AND targets.username = (?)
        AND sources.id = follows.source_id AND targets.id = follows.target_id
    |sql}

let destroy (module Db : Caqti_lwt.CONNECTION) ~source ~target =
  let result = Db.exec destroy_query (source, target) in
  Ocoi.Db.handle_caqti_result result

let migrate_query =
  Caqti_request.exec Caqti_type.unit
    {sql| CREATE TABLE follows (
         source_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
         target_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
         PRIMARY KEY (source_id, target_id)
       )
    |sql}

let migrate (module Db : Caqti_lwt.CONNECTION) = Db.exec migrate_query ()

let rollback_query =
  Caqti_request.exec Caqti_type.unit {sql| DROP TABLE follows |sql}

let rollback (module Db : Caqti_lwt.CONNECTION) = Db.exec rollback_query ()
