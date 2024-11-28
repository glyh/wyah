open Parser

let%test "simple" =
  parse_string "main = S K K 3;"
  = [
      {
        name = "main";
        args = [];
        body = Ap (Ap (Ap (Var "S", Var "K"), Var "K"), Num 3);
      };
    ]
