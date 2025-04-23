set -e
redo-ifchange compile_csharp compile_grammar
dotnet build
