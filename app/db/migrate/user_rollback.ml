
let result = Lwt_main.run (Queries.User.rollback Db.connection)

let () =
  match result with
  | Ok () -> print_endline "Rollback successful."
  | Error err ->
      print_endline "Rollback failed!" ;
      failwith (Caqti_error.show err)
