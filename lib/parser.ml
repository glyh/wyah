include Nice_parser.Make(struct
  type result = Ast.expr
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let parse = Menhir_parser.expr_eof
  include Lexer
end)
