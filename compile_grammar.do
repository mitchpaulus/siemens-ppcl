redo-ifchange "$ANTLR_JAR" PPCL.g4
java -jar "$ANTLR_JAR" -o grammar_test/ -Dlanguage=Java PPCL.g4
javac grammar_test/*.java
