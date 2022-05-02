﻿using System;
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

namespace SiemensPPCL;

public class Program
{
    public static void Main(string[] argv)
    {
        int i = 0;

        string filepath = "";
        while (i < argv.Length)
        {
            if (argv[i] == "-h")
            {
                Console.Write(HelpText());
                Environment.ExitCode = 0;
                return;
            }
            else
            {
                filepath = argv[i];
            }

            i++;
        }

        if (string.IsNullOrWhiteSpace(filepath))
        {
            Console.Error.Write("No file argument passed.\n");
            Environment.ExitCode = 1;
            return;
        }

        string fileContents = File.ReadAllText(filepath, Encoding.UTF8);

        AntlrInputStream inputStream = new AntlrInputStream(fileContents);

        PPCLLexer lexer = new PPCLLexer(inputStream);

        lexer.RemoveErrorListeners();
        var lexerErrorListener = new AntlrErrorListener();
        lexer.AddErrorListener(lexerErrorListener);

        CommonTokenStream tokens = new CommonTokenStream(lexer);

        PPCLParser parser = new PPCLParser(tokens);
        var parserErrorListener = new AntlrErrorListener();
        parser.RemoveErrorListeners();
        parser.AddErrorListener(parserErrorListener);

        PPCLParser.ProgramContext? tree = parser.program();

        //GotoListener listener = new GotoListener();

        PPCLListener listener = new();

        ParseTreeWalker walker = new ParseTreeWalker();
        walker.Walk(listener, tree);

        if (lexerErrorListener.Errors.Any() || parserErrorListener.Errors.Any())
        {
            Environment.ExitCode = 1;
            return;
        }

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
    }

    public static List<string> AllPoints(IParseTree context, List<string> currentList)
    {
        for (var i = 0; i < context.ChildCount; i++)
        {
            IParseTree child = context.GetChild(i);
            if (child.GetType() == typeof(TerminalNodeImpl))
            {
                IToken? node = ((TerminalNodeImpl) child).Symbol;
                if (node.Type == PPCLLexer.POINT) currentList.Add(node.Text);
            }
            else
            {
                AllPoints(child, currentList);
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


    public static string HelpText()
    {
        StringBuilder builder = new StringBuilder();

        builder.Append("Siemens-PPCL.exe\n");
        builder.Append("\n");
        builder.Append("USAGE:\n");
        builder.Append("Siemens-PPCL.exe file\n");
        builder.Append("\n");
        builder.Append("Analyzes a file for 'ALMACK' points.\n");
        return builder.ToString();
    }
}