{ 			      	 	  			 	
  open Lexing
  open Parser
  open Printf
 			      	 	  			 	  
  exception Eof
  exception Syntax_err of string
 			      	 	  			 	      
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
      }

  let add_token _ _ = ()
 			      	 	  			 	      
  let keyword_table = Hashtbl.create 42
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
              [ "def", DEF;
		"let", LET;
		"while", WHILE;
		"if", IF;
		"then", THEN;
		"else", ELSE;
		"ref", REF;
		"int", INT;
		"float", FLOAT;
		"bool", BOOL;
		"true", BOOLCONST(true);
		"false", BOOLCONST(false);
		"unit", UNIT;
		"tt", TT;
		"not", NOT;
		"in", IN ]

}

let newline = '\r' | '\n' | "\r\n"
 			      	 	  			 	
rule token = parse
    "//"       { comment lexbuf }
  | "(*"       { nested_comment 0 lexbuf }
  | [' ' '\t']                  { token lexbuf }
  | newline                     { next_line lexbuf; token lexbuf }
  | ['0'-'9']+'.'['0'-'9']* as lxm
        { add_token ("FLOATCONST(" ^ lxm ^ ")") lexbuf;
        FLOATCONST(float_of_string lxm) }
  | ['0'-'9']+ as lxm
      { add_token ("INTCONST(" ^ lxm ^ ")") lexbuf;
        INTCONST(Int32.of_string lxm) }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
               { try
                   let id' = Hashtbl.find keyword_table id in
		   add_token id lexbuf;
		   id'
                 with Not_found ->
                   add_token ("ID(" ^ id ^ ")") lexbuf;
                   ID id
	       }
  | '!'        { add_token "DEREF" lexbuf; DEREF }
  | '+'        { add_token "PLUS" lexbuf; PLUS }
  | '-'        { add_token "MINUS" lexbuf; MINUS }
  | '*'        { add_token "TIMES" lexbuf; TIMES }
  | '/'        { add_token "DIV" lexbuf; DIV }
  | "&&"       { add_token "AND" lexbuf; AND }
  | "||"       { add_token "OR" lexbuf; OR }
  | '<'        { add_token "LT" lexbuf; LT }
  | "=="       { add_token "INT_EQ" lexbuf; INT_EQ }
  | ":="       { add_token "DEFEQ" lexbuf; DEFEQ }
  | '('        { add_token "LPAREN" lexbuf; LPAREN }
  | ')'        { add_token "RPAREN" lexbuf; RPAREN }
  | '{'        { add_token "LBRACE" lexbuf; LBRACE }
  | '}'        { add_token "RBRACE" lexbuf; RBRACE }
  | ';'        { add_token "SEMI" lexbuf; SEMI }
  | '='        { add_token "EQ" lexbuf; EQ }
  | ':'        { add_token "COLON" lexbuf; COLON }
  | ','        { add_token "COMMA" lexbuf; COMMA }
  | eof        { add_token "EOF" lexbuf; EOF }
  | _          { raise (Syntax_err ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 			      	 	  			 	
and comment = parse
    newline    { next_line lexbuf; token lexbuf }
  | eof        { EOF }
  | _          { comment lexbuf }
 			      	 	  			 	
and nested_comment level = parse
    newline    { next_line lexbuf; nested_comment level lexbuf }
  | eof        { raise (Syntax_err "Unclosed comment") }
  | "(*"       { nested_comment (level+1) lexbuf }
  | "*)"       { if level = 0 then token lexbuf
    	       	 else nested_comment (level-1) lexbuf
               }
  | _          { nested_comment level lexbuf }
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
 			      	 	  			 	
  

