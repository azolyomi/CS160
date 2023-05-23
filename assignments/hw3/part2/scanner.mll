{ 
  open Parser 
  
  (* Increment line number.
   * Note: incrementing column number is handled automatically
   *)
  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

rule token = parse
 | [' ' '\r' '\t'] { token lexbuf }
 | '\n'            { incr_linenum lexbuf; token lexbuf}
 | ['0'-'9']+ as n { NUMBER (int_of_string n) }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "while"         { WHILE }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "="             { ASSIGN }
  | "+"             { PLUS }
  | "-"             { MINUS }
  | "*"             { TIMES }
  | "/"             { DIV }
  | "<"             { LT }
  | ">"             { GT }  
  | "<="            { LEQ }
  | ">="            { GEQ }
  | "=="            { EQ }
  | "!="            { NEQ }
  | "&&"            { AND }
  | "||"            { OR }
  | "!"             { NOT }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "let"           { LET }
  | ":"             { COLON }
  (* type *)
  | "unit"         { UNIT_TYPE }
  | "bool"         { BOOL_TYPE }
  | "int"          { INT_TYPE }
  | "[int]"        { INTARR_TYPE }
  (* function *)
  | "fn"           { FN }
  | "->"           { ARROW }
  | ['a'-'z' '_']['a'-'z' '0'-'9' 'A'-'Z' '_']* as n { ID (n) }

 | "//"            { comment lexbuf }
 | eof             { EOF }

and comment = parse
 | '\n'            { incr_linenum lexbuf; token lexbuf }
 | _               { comment lexbuf }
