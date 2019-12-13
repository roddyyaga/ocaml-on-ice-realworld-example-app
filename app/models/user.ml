type t = {
  id: int;
  username: string;
  email: string;
  password_hash: string;
  bio: string option;
  image: string option;
}
[@@deriving yojson]
