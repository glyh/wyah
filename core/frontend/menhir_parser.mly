%{
  [@@@coverage exclude_file]
  open Ast

%}

%token EOF

%token <int> INT

%token BOR BAND EQ NE GE GT LE LT
%token ADD SUB MUL DIV

%left BOR
%left BAND
%left EQ NE GE GT LE LT
%left ADD SUB
%left MUL DIV

%token LETREC LET IN CASE OF PACK

%token FSLASH DOT

%token LANG RANG RARROW

%token LPAREN RPAREN LBRACE RBRACE COMMA

%token SEMICOL ASSIGN 

%token <string> IDENT 

%start <name definition list> toplevel_eof

%%

toplevel_eof:
  | scs=nonempty_list(supercombinator) EOF { scs }

supercombinator:
  | name=IDENT args=list(IDENT) ASSIGN body=expr SEMICOL {
    {
      name;
      args;
      body;
    }
  }

%inline bin_op:
  | BOR  { fun lhs rhs -> Ap(Ap(Prim Bor , lhs), rhs) }
  | BAND { fun lhs rhs -> Ap(Ap(Prim Band, lhs), rhs) }
  | EQ   { fun lhs rhs -> Ap(Ap(Prim Eq  , lhs), rhs) }
  | NE   { fun lhs rhs -> Ap(Ap(Prim Ne  , lhs), rhs) }
  | GE   { fun lhs rhs -> Ap(Ap(Prim Ge  , lhs), rhs) }
  | GT   { fun lhs rhs -> Ap(Ap(Prim Gt  , lhs), rhs) }
  | LE   { fun lhs rhs -> Ap(Ap(Prim Le  , lhs), rhs) }
  | LT   { fun lhs rhs -> Ap(Ap(Prim Lt  , lhs), rhs) }
  | ADD  { fun lhs rhs -> Ap(Ap(Prim Add , lhs), rhs) }
  | SUB  { fun lhs rhs -> Ap(Ap(Prim Sub , lhs), rhs) }
  | MUL  { fun lhs rhs -> Ap(Ap(Prim Mul , lhs), rhs) }
  | DIV  { fun lhs rhs -> Ap(Ap(Prim Div , lhs), rhs) }

expr:
  | f=expr x=aexpr {
    Ap(f, x)
  }
  | lhs=expr op=bin_op rhs=expr {
    op lhs rhs
  }
  | LET bindings=separated_nonempty_list(SEMICOL, defn) IN body=expr {
    Let {
      recursive = false;
      bindings;
      body;
    }
  }
  | LETREC bindings=separated_nonempty_list(SEMICOL, defn) IN body=expr {
    Let {
      recursive = true;
      bindings;
      body;
    }
  }
  | CASE condition=expr OF branches=separated_nonempty_list(SEMICOL, alt) {
    Case {
      condition;
      branches;
    }
  }
  | FSLASH args=list(IDENT) DOT body=expr {
    List.fold_right
      (fun arg body -> Lam(arg, body))  
      args
      body
  }
  | aexpr=aexpr {
    aexpr
  }

aexpr:
  | var=IDENT { Var(var) }
  | num=INT { Num(num) }
  | PACK LBRACE tag=INT COMMA arity=INT RBRACE { Constr(tag, arity) }
  | LPAREN expr=expr RPAREN { expr }

defn:
  | var=IDENT ASSIGN body=expr { (var, body) }


alt: 
  | LANG arity=INT RANG fields=list(IDENT) RARROW body=expr {
    (arity, fields, body)
  }
