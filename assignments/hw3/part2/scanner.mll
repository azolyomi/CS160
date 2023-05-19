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
 | ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] { INT() }
 | "//"            { comment lexbuf }
 | eof             { EOF }

and comment = parse
 | '\n'            { incr_linenum lexbuf; token lexbuf }
 | _               { comment lexbuf }
