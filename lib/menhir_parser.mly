%{
  [@@@coverage exclude_file]
  open Ast
  (*open Type_environment*)

%}

%token EOF

%token TRUE FALSE UNIT
%token <int> INT
%token <char> CHAR

%token ADD SUB EQ
%left EQ
%left ADD SUB

%token IF THEN ELSE
%token LPAREN RPAREN
%token FSLASH (*DOT COLON*)
%token RARROW (*T_INT T_BOOL T_CHAR T_IO*)
%token LET REC ASSIGN IN

%token <string> IDENT 

%start <top_level> toplevel_eof

%%

toplevel_eof:
  | e=expr EOF { Expr e }
  | d=definition EOF { Definition d }

definition: 
  | id_lam=let_lambda {
    let (id, lam) = id_lam in
    (id, lam) 
  }
  | name=IDENT ASSIGN e=expr { (name, e) }

expr: 
  | e=expr_lam { e } 

(*type_sig:*)
(*  | t=type_sig_arrow { t }*)
(**)
(*type_sig_arrow:*)
(*  | lhs=type_sig_monad RARROW rhs=type_sig_arrow { TArrow(lhs, rhs) }*)
(*  | t=type_sig_monad { t }*)
(**)
(*type_sig_monad:*)
(*  | T_IO inner=type_sig_primary { TIO(inner) }*)
(*  | t=type_sig_primary { t }*)
(**)
(*type_sig_primary:*)
(*  | T_INT { t_int }*)
(*  | T_BOOL { t_bool }*)
(*  | T_CHAR { t_char }*)
(*  | UNIT { t_unit }*)
(*  | LPAREN t=type_sig RPAREN { t }*)

let_syntax:
  | LET id=IDENT params=list(IDENT) ASSIGN {
    (id, params, false)
  }
  | LET REC id=IDENT params=list(IDENT) ASSIGN {
    (id, params, true)
  }

let_lambda:
  | ls=let_syntax rhs=expr_lam {
    let (id, params, is_recursive) = ls in
    let rhs_wrapped = 
      List.fold_right
      (fun p inner -> Lam(p, inner)) 
      params
      rhs
    in
    if is_recursive
    then
      (id, Fix(Lam(id, rhs_wrapped)))
    else
      (id, rhs_wrapped)
  }


expr_lam:
  | FSLASH param=IDENT params=list(IDENT) RARROW body=expr_lam {
    List.fold_right
    (fun p inner -> Lam(p, inner))
    (param :: params)
    body
  }
  | id_lam=let_lambda IN inner=expr_lam {
    let (id, lam) = id_lam in
      LetIn(id, lam, inner)
  }
  | e=expr_stmt { e }


expr_stmt:
  | IF test=expr_stmt THEN then_clause=expr_stmt ELSE else_clause=expr_stmt {
    If(test, then_clause, else_clause)
  }
  | e=expr_expr { e }


%inline bin_op:
  | ADD { "+" }
  | SUB { "-" }
  | EQ { "==" }
  
expr_expr: 
  | lhs=expr_expr op=bin_op rhs=expr_expr {
    App(App(Var op, lhs), rhs)
  }
  | e=expr_app { e }

expr_app: 
  | f=expr_primary xs=list(expr_primary) {
    List.fold_left 
    (fun partial arg -> App(partial, arg))
    f
    xs
  }

expr_primary:
  | i=INT { Atom(Int i) }
  | c=CHAR { Atom(Char c) }
  | TRUE { Atom(Bool true) }
  | FALSE { Atom(Bool false) }
  | UNIT { Atom(Unit) }
  | id=IDENT {
    Var(id)
  }
  | LPAREN e=expr RPAREN { e }
