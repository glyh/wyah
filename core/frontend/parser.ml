include Nice_parser.Make (struct
  type result = Ast.name Ast.definition list
  type token = Menhir_parser.token

  exception ParseError = Menhir_parser.Error

  let parse = Menhir_parser.toplevel_eof

  include Lexer
end)
