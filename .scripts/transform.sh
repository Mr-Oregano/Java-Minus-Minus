cd $DOMAIN/Transformation/bin 

# Transform
./transform.exe "@MLton" "load-world" "$DOMAIN\Transformation\bin\transform.mlton" -- --dir="$DOMAIN\Transformation" --tlp=$1 --target-dir="$DOMAIN\Target" --target-index=0 --target-type=single $2