# Java-Minus-Minus

The *Java Minus Minus* language, sort of a subset of Java but not really. The best language of them all. Why need functions when you can have *Java Minus Minus*?

### Building
##

Probably not going to be able to build this unless you have the TLSystem.
Also, requires MLTon SML compiler: http://mlton.org/. 

  - Run `.scripts/build.sh <bnf> <start_symbol>` to build all (parser, transformer, and pretty printer) where `<bnf>` is your grammar and `<start_symbol>` is the start symbol for that grammar. 
 Create an env variable called `DOMAIN` and set it to the root of this project (folder containing `.git`). You should also set `MLTON_BIN` and `TL_SYSTEM` to the MLTon/bin and TLSystem path respectively.

  - Run `.scripts/tlp.sh <tlp>` to build TLP, where `<tlp>` is the name of the TLP file (without extension).

  - Run `.scripts/transform.sh <tlp> <target>` to interpret a program, where `<tlp>` is the name of the TLP file (without extension) and `<target>` is the name of your target program.