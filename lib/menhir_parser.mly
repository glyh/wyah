%{
  [@@@coverage exclude_file]
  open Ast

%}

%token EOF

%token TRUE FALSE
%token <int> INT

%token IF THEN ELSE
%token LPAREN RPAREN

%token <string> IDENT 

%start <expr> expr_eof

%%

expr_eof:
  | e=expr_stmt EOF { e }

expr_stmt:
  | IF test=expr_stmt THEN then_clause=expr_stmt ELSE else_clause=expr_stmt {
    If(test, then_clause, else_clause)
  }
  | e=expr_expr { e }


expr_expr: 
  | f=expr_primary xs=list(expr_primary) {
    List.fold_left 
    (fun partial arg -> App(partial, arg))
    f
    xs
  }

expr_primary:
  | i=INT { Val(Int i) }
  | TRUE { Val(Bool true) }
  | FALSE { Val(Bool false) }
  | id=IDENT {
    Ident(id)
  }
  | LPAREN e=expr_stmt RPAREN { e }
