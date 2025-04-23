set -e
redo-ifchange "$ANTLR_JAR" PPCL.g4
java -jar "$ANTLR_JAR" -o Siemens-PPCL/antlr/ -Dlanguage=CSharp PPCL.g4
