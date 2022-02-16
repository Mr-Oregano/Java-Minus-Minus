cp $DOMAIN/Transformation/Syntax/lexer.spec $DOMAIN/Transformation/bin/ && mv $DOMAIN/Transformation/bin/lexer.spec $DOMAIN/Transformation/bin/target_tokens.spec
$MLTON_BIN/mlyacc.exe "$TL_SYSTEM\Engine\ParserGenerator\parser\ReadGrammar\grammar.grm"
$MLTON_BIN/mllex.exe "$TL_SYSTEM\Engine\ParserGenerator\lexer\tl_lex.spec"
$MLTON_BIN/mllex.exe "$DOMAIN\Transformation\bin\target_tokens.spec"
$MLTON_BIN/mlton.bat -mlb-path-var "TL $TL_SYSTEM" -mlb-path-var "DOMAIN $DOMAIN" -output "$DOMAIN\Transformation\bin\parser.exe" -verbose 1 -const "Exn.keepHistory false" -profile no -profile-branch false -profile-stack false -profile-val false "$TL_SYSTEM\Parse\parser.mlb"
(cd $TL_SYSTEM/Parse && $DOMAIN/Transformation/bin/parser.exe "$DOMAIN\Transformation" $1 $2)