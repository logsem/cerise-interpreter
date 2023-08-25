let explode_string s = List.init (String.length s) (String.get s)
let implode_string s = String.concat "" (List.map (String.make 1) s)
