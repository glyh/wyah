%{
  [@@@coverage exclude_file]
  open Ast

%}

%token EOF

%token <int> INT

%token BOR BAND
%token CONS
%token EQ NE GE GT LE LT
%token ADD SUB MUL DIV

%left BOR
%left BAND
%right CONS
%left EQ NE GE GT LE LT
%left ADD SUB
%left MUL DIV

%token LETREC LET IN CASE PACK

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

expr:
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
  | CASE condition=expr LBRACE branches=nonempty_list(alt) RBRACE {
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
  | eexpr=eexpr {
    eexpr
  }

%inline bin_op:
  | BOR  { fun lhs rhs -> Ap(Ap(Var "or"  , lhs), rhs) }
  | BAND { fun lhs rhs -> Ap(Ap(Var "and"  , lhs), rhs) }
  | EQ   { fun lhs rhs -> Ap(Ap(Var "==" , lhs), rhs) }
  | NE   { fun lhs rhs -> Ap(Ap(Var "~=" , lhs), rhs) }
  | GE   { fun lhs rhs -> Ap(Ap(Var ">=" , lhs), rhs) }
  | GT   { fun lhs rhs -> Ap(Ap(Var ">"  , lhs), rhs) }
  | LE   { fun lhs rhs -> Ap(Ap(Var "<=" , lhs), rhs) }
  | LT   { fun lhs rhs -> Ap(Ap(Var "<"  , lhs), rhs) }
  | ADD  { fun lhs rhs -> Ap(Ap(Var "+"  , lhs), rhs) }
  | SUB  { fun lhs rhs -> Ap(Ap(Var "-"  , lhs), rhs) }
  | MUL  { fun lhs rhs -> Ap(Ap(Var "*"  , lhs), rhs) }
  | DIV  { fun lhs rhs -> Ap(Ap(Var "//" , lhs), rhs) }
  | CONS  { fun lhs rhs -> Ap(Ap(Var "Cons" , lhs), rhs) }

eexpr:
  | lhs=eexpr op=bin_op rhs=eexpr {
    op lhs rhs
  }
  | eexpr2=eexpr2 {
    eexpr2
  }

eexpr2:
  | f=eexpr2 x=aexpr {
    Ap(f, x)
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
