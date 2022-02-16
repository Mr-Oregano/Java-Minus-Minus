mkdir -p $DOMAIN/Transformation/bin

# Build Lexer & Parser
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}") 
sh $SCRIPT_RELATIVE_DIR/build-parser.sh $1 $2

# Build: Transformer
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}") 
sh SCRIPT_RELATIVE_DIR/build-transformer.sh $1 $2

# Build: Pretty Printer
SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}") 
sh SCRIPT_RELATIVE_DIR/build-prettyprinter.sh

