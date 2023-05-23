%{
(* Make available names in the Ast module *)
open Ast

(* Report a syntax error with the location of its orgin *)
let syntax_error () =
  let start_pos = Parsing.rhs_start_pos 1 in
  let end_pos = Parsing.rhs_end_pos 1 in
  raise (Error.SyntaxError {
    sl = start_pos.pos_lnum;
    sc = start_pos.pos_cnum - start_pos.pos_bol;
    el = end_pos.pos_lnum;
    ec = end_pos.pos_cnum - end_pos.pos_bol;
  })
%}

/* Tokens */

%token EOF 
%token UNIT
%token <int> NUMBER
%token IF THEN ELSE WHILE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA
%token PLUS MINUS TIMES DIV
%token LT GT LEQ GEQ EQ NEQ
%token AND OR NOT
%token TRUE FALSE
%token LET ASSIGN
%token <string> ID
%token COLON UNIT_TYPE BOOL_TYPE INT_TYPE INTARR_TYPE
%token FN ARROW




// TODO: Your associativity rules here
%right ASSIGN
%left OR
%left AND 
%left PLUS MINUS
%left TIMES DIV
%nonassoc LT GT LEQ GEQ EQ NEQ
%nonassoc UMINUS NOT

%start main
%type <Ast.prog> main
%%

main:
    | prog EOF                          { $1 }
    | error EOF                         { syntax_error () }
prog:
    // |                                   { [] }
    // | <production>                      { <action> } */
    | function_plus { $1 }
function_plus:
    | func { [$1] }
    | func function_plus { $1 :: $2 }
func:
    | FN ID LPAREN param_star_or_empty RPAREN ARROW argtype seq { { name = $2; param = $4; body = $8; return = $7 } }
param_star_or_empty: 
    | param_star {$1}
    | {[]}
param_star:
    | param { [$1] }
    | param COMMA param_star { $1 :: $3 }
param:
    | ID COLON argtype { ($1, $3) }
argtype:
    | UNIT_TYPE                         { TUnit }
    | BOOL_TYPE                         { TBool }
    | INT_TYPE                          { TInt }
    | INTARR_TYPE                       { TArr }
seq: 
    | LBRACE expr_plus RBRACE               { Seq $2 }
expr_plus:
    | expr                                  { [$1] }
    | expr SEMICOLON expr_plus              { $1 :: $3 }
expr: 
    | LPAREN RPAREN                     { Const CUnit }
    | TRUE                              { Const (CBool true) }
    | FALSE                             { Const (CBool false) }
    | NUMBER                            { Const (CInt $1) }
    | ID                                { Id $1 }
    | LPAREN expr RPAREN                { $2 }
    | MINUS expr %prec UMINUS           { Binary (Sub, Const (CInt 0), $2) }
    | NOT expr                          { Unary (Not, $2) }
    | expr PLUS expr                    { Binary (Add, $1, $3) }
    | expr MINUS expr                   { Binary (Sub, $1, $3) }
    | expr TIMES expr                   { Binary (Mul, $1, $3) }
    | expr DIV expr                     { Binary (Div, $1, $3) }
    | expr LT expr                      { Binary (Lt, $1, $3) }
    | expr GT expr                      { Binary (Gt, $1, $3) }
    | expr LEQ expr                     { Binary (Leq, $1, $3) }
    | expr GEQ expr                     { Binary (Geq, $1, $3) }
    | expr EQ expr                      { Binary (Eq, $1, $3) }
    | expr NEQ expr                     { Binary (Neq, $1, $3) }
    | expr AND expr                     { Binary (And, $1, $3) }
    | expr OR expr                      { Binary (Or, $1, $3) }
    | seq                               { $1 }
    | IF expr THEN seq ELSE seq         { Ite ($2, $4, $6) }
    | LET ID COLON argtype ASSIGN expr { Let ($2, $4, $6) }
    | WHILE expr seq                    { While ($2, $3) }
    | ID LBRACKET expr RBRACKET         { Read ($1, $3) }
    | ID ASSIGN expr                    { Assign ($1, $3) }
    | ID LBRACKET expr RBRACKET ASSIGN expr { Write ($1, $3, $6) }
    | ID LPAREN expr_star RPAREN             { Call ($1, $3) }
expr_plus_comma:
    | expr                              { [$1] }
    | expr COMMA expr_plus              { $1 :: $3 }
expr_star:
    | {[]}
    | expr { [$1] }
    | expr COMMA expr_plus_comma { $1 :: $3 }


