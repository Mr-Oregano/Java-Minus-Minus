mkdir -p $DOMAIN/Transformation/bin

# Build Lexer & Parser
cp $DOMAIN/Transformation/Syntax/lexer.spec $DOMAIN/Transformation/bin/ && mv $DOMAIN/Transformation/bin/lexer.spec $DOMAIN/Transformation/bin/target_tokens.spec
$MLTON_BIN/mlyacc.exe "$TL_SYSTEM\Engine\ParserGenerator\parser\ReadGrammar\grammar.grm"
$MLTON_BIN/mllex.exe "$TL_SYSTEM\Engine\ParserGenerator\lexer\tl_lex.spec"
$MLTON_BIN/mllex.exe "$DOMAIN\Transformation\bin\target_tokens.spec"
$MLTON_BIN/mlton.bat -mlb-path-var "TL $TL_SYSTEM" -mlb-path-var "DOMAIN $DOMAIN" -output "$DOMAIN\Transformation\bin\parser.exe" -verbose 1 -const "Exn.keepHistory false" -profile no -profile-branch false -profile-stack false -profile-val false "$TL_SYSTEM\Parse\parser.mlb"
(cd $TL_SYSTEM/Parse && $DOMAIN/Transformation/bin/parser.exe "$DOMAIN\Transformation" $1 $2)

# Build: Transformer
cp $DOMAIN/Transformation/Syntax/format.sty $DOMAIN/Transformation/bin/format.sty && mv $DOMAIN/Transformation/bin/format.sty $DOMAIN/Transformation/bin/format.sty.sml
$MLTON_BIN/mlton.bat -mlb-path-var "TL $TL_SYSTEM" -mlb-path-var "DOMAIN $DOMAIN" -output "$DOMAIN\Transformation\bin\transform.exe" -verbose 1 -const "Exn.keepHistory false" -profile no -profile-branch false -profile-stack false -profile-val false "$TL_SYSTEM\Transform\transform.mlb"
$DOMAIN/Transformation/bin/transform.exe --dir="$DOMAIN\Transformation" --grammar=$1 --start-symbol=$2

# Build: Pretty Printer
$MLTON_BIN/mlton.bat -mlb-path-var "TL $TL_SYSTEM" -mlb-path-var "DOMAIN $DOMAIN" -output "$DOMAIN\Transformation\bin\prettyprint.exe" -verbose 1 -const "Exn.keepHistory false" -profile no -profile-branch false -profile-stack false -profile-val false "$TL_SYSTEM\PrettyPrint\prettyprint.mlb"

