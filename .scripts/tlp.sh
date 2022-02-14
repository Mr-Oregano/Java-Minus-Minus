cd $DOMAIN/Transformation/bin

# Parse TLP
./parser.exe "@MLton" "load-world" "$DOMAIN\Transformation\bin\parser.mlton" -- TLP "$DOMAIN\Transformation" $1