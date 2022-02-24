datatype lexresult  = SHELL of string * string * {line: word, column: word};
val error           = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof             = fn () => SHELL("","eof",getNextTokenPos(""))
val counter         = ref 0;

(* Remove front and back characters from string *)
fun removeFrontAndBack( yytext ) =     
    let
        fun pop( [] ) = raise General.Fail("drop(): provided list was empty.")
          | pop( chars ) = List.take( chars, List.length(chars) - 1 )
    in
        implode(pop(List.tl(explode yytext)))
    end;

%%
%header (functor Target_LexFn(val getNextTokenPos : string -> {line: word, column: word}));


TYPE         = int | bool | string;
INT_LITERAL  = 0 | [1-9][0-9]*;
STR_LITERAL  = \"[^\"\n]*\";
IDENTIFIER   = [_a-zA-Z][_a-zA-Z0-9]*;

ws           = [\  \t \n];

%s COMMENT SINGLE_COMMENT;
%%

<COMMENT> "/*"                      => ( counter := !counter + 1; getNextTokenPos(yytext); lex() );
<COMMENT> "*/"                      => ( counter := !counter - 1; 
                                         if !counter = 0 then YYBEGIN INITIAL 
                                         else (); 
                                         getNextTokenPos(yytext); lex() );

<COMMENT> "\n"                      => ( getNextTokenPos(yytext); lex() );
<COMMENT> .                         => ( getNextTokenPos(yytext); lex() );

<SINGLE_COMMENT> "\n"               => ( YYBEGIN INITIAL; getNextTokenPos(yytext); lex() );
<SINGLE_COMMENT> .                  => ( getNextTokenPos(yytext); lex() );

<INITIAL> "/*"                      => ( YYBEGIN COMMENT; 
                                         counter := !counter + 1; 
                                         getNextTokenPos(yytext); lex() );

<INITIAL> "//"                      => ( YYBEGIN SINGLE_COMMENT; getNextTokenPos(yytext); lex() );

<INITIAL> ";"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> ","                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "("                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> ")"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "{"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "}"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "<"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> ">"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "<="                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> ">="                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "=="                      => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "="                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "!="                      => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "+"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "++"                      => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "-"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "--"                      => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "~"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "*"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "/"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "%"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "^"                       => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "and"	                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "or"	                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "xor"	                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "not"	                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "abs"	                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "println"                 => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "print"                   => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "else"                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "if"                      => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "while"                   => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "for"                     => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> "true"                    => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );
<INITIAL> "false"                   => ( SHELL(yytext, yytext, getNextTokenPos(yytext)) );

<INITIAL> {TYPE}                    => ( SHELL("TYPE", yytext, getNextTokenPos(yytext)) );
<INITIAL> {STR_LITERAL}             => ( SHELL("STR_LITERAL" , removeFrontAndBack(yytext), getNextTokenPos(yytext)) );
<INITIAL> {INT_LITERAL}             => ( SHELL("INT_LITERAL", yytext, getNextTokenPos(yytext)) );
<INITIAL> {IDENTIFIER}              => ( SHELL("IDENTIFIER", yytext, getNextTokenPos(yytext)) );

<INITIAL> {ws}+                     => ( getNextTokenPos(yytext); lex() );