%{
  [@@@coverage exclude_file]
  open Ast

%}

%token EOF

%token TRUE FALSE
%token <int> INT

%token IF THEN ELSE
%token LPAREN RPAREN
%token FSLASH DOT COLON
%token RARROW T_INT T_BOOL
%right RARROW

%token <string> IDENT 

%start <expr> expr_eof

%%

expr_eof:
  | e=expr EOF { e }

expr: 
  | e=expr_lam { e } 

type_sig:
  | T_INT { TInt }
  | T_BOOL { TBool }
  | lhs=type_sig RARROW rhs=type_sig { TArrow(lhs, rhs) }
  | LPAREN t=type_sig RPAREN { t }

expr_lam:
  | FSLASH id=IDENT COLON t=type_sig DOT body=expr_lam {
    Val(Lam(id, t, body))
  }
  | e=expr_stmt { e }


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
    Var(id)
  }
  | LPAREN e=expr RPAREN { e }
