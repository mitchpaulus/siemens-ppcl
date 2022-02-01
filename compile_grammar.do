redo-ifchange /usr/local/lib/antlr-4.9.3-complete.jar PPCL.g4
java -jar /usr/local/lib/antlr-4.9.3-complete.jar -o grammar_test/ -Dlanguage=Java PPCL.g4
javac grammar_test/*.java
