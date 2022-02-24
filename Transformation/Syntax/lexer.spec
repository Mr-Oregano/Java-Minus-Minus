(* ============================================================================================== *) 
datatype lexresult  = SHELL of string * string * {line: word, column: word};
val error           = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof             = fn () => SHELL("","eof",getNextTokenPos(""))
val counter         = ref 0;
(* ============================================================================================== *)
(* ------------------------------------------------------------------ *)
(* assumes that ">" does not occur as part of a nonterminal symbol *)
fun generateSchemaTokenName( yytext ) =
    let
        fun split(x, []   ) =  raise General.Fail("an_error")
          | split(x, y::ys) = if x=y then ys else split(x,ys);
                                                    
        fun splitFirst(symbol,[])    =     [] (* symbol was not in the input list *)
          | splitFirst(symbol,x::xs) =     if x = symbol 
                        then (* found split point *)
                            []
                        else (* keep looking      *)
                            x::splitFirst(symbol,xs);
                                                                        
        val s0   = explode(yytext);
        val s1   = split(#"<",s0);
        val s2   = splitFirst(#">",s1);  
    in
        implode(explode("!#schema_variable_") @ s2)        
    end;

fun removeFrontAndBack( yytext ) =     
    let
        fun pop( [] ) = raise General.Fail("drop(): provided list was empty.")
          | pop( chars ) = List.take( chars, List.length(chars) - 1 )
    in
        implode(pop(List.tl(explode yytext)))
    end;
(* ============================================================================================== *)
%%
%header (functor Target_LexFn(val getNextTokenPos : string -> {line: word, column: word}));

alpha        = [A-Za-z];
digit        = [0-9];
alphanumeric = [A-Za-z0-9_];
ws           = [\  \t \n];
symbol       = [<];

TYPE         = int | bool | string;
INT_LITERAL  = 0 | [1-9][0-9]*;
STR_LITERAL  = \"[^\"\n]*\";
IDENTIFIER   = [_a-zA-Z][_a-zA-Z0-9]*;

string       = \"{alphanumeric}*\";
symbolic_id  = {symbol}+{alphanumeric}*;
schema_id    = "<" {alpha}{alphanumeric}* ">_" {alphanumeric}+;
ws           = [\  \t \n];

%s COMMENT;
%s SINGLE_COMMENT;
%%

<COMMENT> "/*"                      => ( counter := !counter + 1; getNextTokenPos(yytext); lex()         );
<COMMENT> "*/"                      => ( counter := !counter - 1; 
                                         if !counter = 0 then YYBEGIN INITIAL 
                                         else (); 
                                         getNextTokenPos(yytext); lex()                                  );

<COMMENT> "\n"                      => ( getNextTokenPos(yytext); lex()                                  );
<COMMENT> .                         => ( getNextTokenPos(yytext); lex()                                  );

<SINGLE_COMMENT> "\n"               => ( YYBEGIN INITIAL; getNextTokenPos(yytext); lex() );
<SINGLE_COMMENT> .                  => ( getNextTokenPos(yytext); lex()                                  );

<INITIAL> "/*"                      => ( YYBEGIN COMMENT; 
                                         counter := !counter + 1; 
                                         getNextTokenPos(yytext); lex()                                  );

<INITIAL> "//"                      => ( YYBEGIN SINGLE_COMMENT; getNextTokenPos(yytext); lex() );

<INITIAL> ";"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ","                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "("                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ")"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "{"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "}"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "<"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> ">"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "=="                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "="                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "!="                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "+"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "++"                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "-"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "--"                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "~"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "*"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "/"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "%"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "^"                       => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "and"	                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "or"	                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "not"	                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "abs"	                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "print"                   => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "else"                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "if"                      => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "while"                   => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "for"                     => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> "true"                    => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "false"                   => ( SHELL(yytext                            , yytext,     getNextTokenPos(yytext))    );

<INITIAL> {TYPE}                    => ( SHELL("TYPE"                            , yytext,     getNextTokenPos(yytext))    );
<INITIAL> {STR_LITERAL}             => ( SHELL("STR_LITERAL" , removeFrontAndBack(yytext),     getNextTokenPos(yytext))    );
<INITIAL> {INT_LITERAL}             => ( SHELL("INT_LITERAL"                     , yytext,     getNextTokenPos(yytext))    );
<INITIAL> {IDENTIFIER}              => ( SHELL("IDENTIFIER"                      , yytext,     getNextTokenPos(yytext))    );

<INITIAL> {ws}+                     => ( getNextTokenPos(yytext); lex()  );
<INITIAL> {schema_id}               => ( SHELL(generateSchemaTokenName(yytext)	 , yytext,     getNextTokenPos(yytext))    );
<INITIAL> "[:]"                     => ( SHELL(""                                , yytext,     getNextTokenPos(yytext))    );