let show_query =
  Caqti_request.find_opt Caqti_type.string
    Caqti_type.(
      let ( & ) = tup2 in
      int & string & string & string & option string & option string)
    {sql| SELECT id, username, email, password_hash, bio, image
          FROM users
          WHERE username = (?)
    |sql}

let show = User.get_one_param show_query
