using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using Siemens_PPCL;

namespace SiemensPPCL
{
    public class Program
    {
        public static void Main(string[] argv)
        {
            string filepath = argv[0];

            string fileContents = File.ReadAllText(filepath, Encoding.UTF8);

            AntlrInputStream inputStream = new AntlrInputStream(fileContents);

            PPCLLexer lexer = new PPCLLexer(inputStream);

            lexer.RemoveErrorListeners();
            IAntlrErrorListener<int> lexerErrorListener = new AntlrErrorListener();
            lexer.AddErrorListener(lexerErrorListener);

            CommonTokenStream tokens = new CommonTokenStream(lexer);

            PPCLParser parser = new PPCLParser(tokens);
            IAntlrErrorListener<IToken> parserErrorListener = new AntlrErrorListener();
            parser.RemoveErrorListeners();
            parser.AddErrorListener(parserErrorListener);

            PPCLParser.ProgramContext? tree = parser.program();

            //GotoListener listener = new GotoListener();

            PPCLListener listener = new();

            ParseTreeWalker walker = new ParseTreeWalker();
            walker.Walk(listener, tree);

            Dictionary<string, string> defines = new Dictionary<string, string>();

            HashSet<string> points = new HashSet<string>();

            foreach (PPCLParser.LineContext line in listener.lines)
            {
                if (line.statement()?.defineStatement() is PPCLParser.DefineStatementContext defineStatement)
                {
                    string firstPoint = defineStatement.POINT()[0].GetText();

                    if (firstPoint[0] == '"')
                    {
                        firstPoint = firstPoint.Substring(1, firstPoint.Length - 2);
                    }

                    string secondPoint = defineStatement.POINT()[1].GetText();

                    if (secondPoint[0] == '"')
                    {
                        secondPoint = secondPoint.Substring(1, secondPoint.Length - 2);
                    }

                    defines[firstPoint] = secondPoint;
                }

                if (line.GetText().Contains("ALMACK"))
                {
                    var pointList = AllPoints(line, new List<string>());

                    foreach (var point in pointList) {
                        var replacedName = RemoveQuotesIfRequired(VariableNameReplacement(point, defines));
                        points.Add(replacedName);
                    }
                }
            }

            foreach (var point in points.OrderBy(s => s)) Console.Write($"{point}\n");

            //Console.Write(listener.builder.ToString());

            //Console.Write("\nUnique Lines:\n");

            //List<int> sortedLines = listener.GotoLocations
            //    .Select(int.Parse)
            //    .OrderBy(i => i).ToList();

            //foreach (int line in sortedLines)
            //{
            //    Console.Write($"{line}\n");
            //}
        }

        public static List<string> AllPoints(IParseTree context, List<string> currentList)
        {
            //Console.Write($"Looping through {context.ChildCount} children\n");
            for (var i = 0; i < context.ChildCount; i++)
            {
                IParseTree child = context.GetChild(i);
                if (child.GetType() == typeof(TerminalNodeImpl))
                {
                    var node = ((TerminalNodeImpl) child).Symbol;
                    if (node.Type == PPCLLexer.POINT)
                    {
                        currentList.Add(node.Text);
                    }
                }
                else
                {
                    currentList.AddRange(AllPoints(child, currentList));
                }
            }

            return currentList;
        }

        public static string VariableNameReplacement(string variableName, Dictionary<string, string> replacements)
        {
            // Replace '%xxx%' placeholders with the value of the variable 'xxx'
            // If the variable is not defined, replace with '%xxx%'
            Regex regex = new Regex(@"%(.*?)%");

            string result = regex.Replace(variableName, match =>
            {
                string variable = match.Groups[1].Value;

                if (replacements.ContainsKey(variable))
                {
                    return replacements[variable];
                }

                return match.Value;
            });

            return result;
        }

        public static string RemoveQuotesIfRequired(string variableName)
        {
            if (variableName[0] == '"')
            {
                return variableName.Substring(1, variableName.Length - 2);
            }

            return variableName;
        }
    }
}
