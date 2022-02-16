mkdir -p $DOMAIN/Transformation/bin

# Build Lexer & Parser
echo -e "\n===================================== BUILD PARSER =====================================\n"
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}") 
sh $SCRIPT_RELATIVE_DIR/build-parser.sh $1 $2

# Build: Transformer
echo -e "\n===================================== BUILD TRANSFORMER =====================================\n"
sh $SCRIPT_RELATIVE_DIR/build-transformer.sh $1 $2

# Build: Pretty Printer
echo -e "\n===================================== BUILD PRETTY PRINTER =====================================\n"
sh $SCRIPT_RELATIVE_DIR/build-prettyprinter.sh

