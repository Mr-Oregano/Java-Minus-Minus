$MLTON_BIN/mlton.bat -mlb-path-var "TL $TL_SYSTEM" -mlb-path-var "DOMAIN $DOMAIN" -output "$DOMAIN\Transformation\bin\prettyprint.exe" -verbose 1 -const "Exn.keepHistory false" -profile no -profile-branch false -profile-stack false -profile-val false "$TL_SYSTEM\PrettyPrint\prettyprint.mlb"