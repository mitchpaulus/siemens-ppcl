using System;
using System.Collections.Generic;
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
        int gosubLine = -1;

        bool printArgPoints = false;
        bool printFilename = false;

        // List of 0 index argument numbers to go along with gosubLine
        List<int> gosubArgs = new List<int>();

        while (i < argv.Length)
        {
            if (argv[i] == "-h" || argv[i] == "--help")
            {
                Console.Write(HelpText());
                Environment.ExitCode = 0;
                return;
            }
            else if (argv[i] == "--gosub")
            {
                // Check for next argument
                if (i + 1 >= argv.Length)
                {
                    Console.Write("Error: Missing argument for --gosub\n");
                    Environment.ExitCode = 1;
                    return;
                }
                // Try parsing the argument as an integer
                if (!int.TryParse(argv[i + 1], out gosubLine))
                {
                    Console.Write($"Error: Could not parse argument '{argv[i + 1]}' as an integer\n");
                    Environment.ExitCode = 1;
                    return;
                }
            }
            else if (argv[i] == "--args")
            {
                // Check for next argument
                if (i + 1 >= argv.Length)
                {
                    Console.Write("Error: Missing argument for --args\n");
                    Environment.ExitCode = 1;
                    return;
                }
                // Try parsing the argument as a list of comma separated integers. Return an error if it fails
                string[] args = argv[i + 1].Split(',');
                foreach (string arg in args)
                {
                    if (!int.TryParse(arg, out int argInt))
                    {
                        Console.Write($"Error: Could not parse argument '{arg}' as an integer\n");
                        Environment.ExitCode = 1;
                        return;
                    }
                    gosubArgs.Add(argInt);
                }
            }
            else if (argv[i] == "--print-arg-points")
            {
                printArgPoints = true;
            }
            else if (argv[i] == "--print-filename" || argv[i] == "-f")
            {
                printFilename = true;
            }
            else if (argv[i] == "--version")
            {
                Console.Write("0.1.0\n");
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

        string fileContents;
        try
        {
            fileContents = File.ReadAllText(filepath, Encoding.UTF8);
        }
        catch (FileNotFoundException)
        {
            Environment.ExitCode = 1;
            Console.Error.Write($"Could not find file '{filepath}'\n");
            return;
        }
        catch (Exception)
        {
            Environment.ExitCode = 1;
            Console.Error.Write($"Could not read text from file '{filepath}'\n");
            return;
        }

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
            Console.Error.Write($"Errors processing '{filepath}'\n");

            foreach (var error in lexerErrorListener.Errors) Console.Error.Write(error);
            foreach (var error in parserErrorListener.Errors) Console.Error.Write(error);

            Environment.ExitCode = 1;
            return;
        }

        Dictionary<string, string> defines = new Dictionary<string, string>();

        HashSet<(string Name, int PpclLineNum)> points = new();

        HashSet<string> localVarsSet = new HashSet<string>();

        foreach (PPCLParser.LineContext line in listener.lines)
        {
            if (line.statement()?.defineStatement() is { } defineStatement)
            {
                string firstPoint  = defineStatement.POINT()[0].GetText().StripQuotes();
                string secondPoint = defineStatement.POINT()[1].GetText().StripQuotes();
                defines[firstPoint] = secondPoint;
            }

            if (line.GetText().Contains("ALMACK"))
            {
                var pointList = AllPoints(line, new List<string>());

                foreach (var point in pointList)
                {
                    var replacedName = VariableNameReplacement(point, defines).StripQuotes();
                    var tuple = (replacedName, int.Parse(line.POS_INT().GetText()));
                    points.Add(tuple);
                }

                List<string> localVars = AllOfType(line, PPCLLexer.LOCALVAR, new List<string>());
                foreach (var localVar in localVars) localVarsSet.Add(localVar);
            }

            if (line.statement()?.gosubStatement() is { } gosubStatement)
            {
                var lineNum = int.Parse(gosubStatement.POS_INT().GetText());

                if (lineNum == gosubLine)
                {
                    foreach (var argNum in gosubArgs)
                    {
                        // Check if the length of the argument list is greater than the argument number
                        if (argNum >= gosubStatement.gosubArgument().Length) {
                            Console.WriteLine($"Error: Argument number {argNum} is out of range for gosub {gosubLine}");
                            Environment.ExitCode = 1;
                            return;
                        }

                        var pointName = gosubStatement.gosubArgument(argNum).GetText();
                        var replacedName = VariableNameReplacement(pointName, defines).StripQuotes();

                        var tuple = (replacedName, int.Parse(line.POS_INT().GetText()));
                        points.Add(tuple);
                    }
                }
            }
        }

        foreach (var (name, ppclLineNum) in points.OrderBy(s => s))
        {
            // Check if point matches '$ARGXX'. If so, don't print it
            if (!printArgPoints && name.StartsWith("$ARG")) continue;

            if (printFilename)
            {
                var filename = Path.GetFileName(filepath);
                Console.Write($"{name}\t{filename}\t{ppclLineNum}\n");
            }
            else
            {
                Console.Write($"{name}\n");
            }

        }
    }

    public static List<string> AllOfType(IParseTree context, HashSet<int> childTypes, List<string> currentList)
    {
        for (var i = 0; i < context.ChildCount; i++)
        {
            IParseTree child = context.GetChild(i);
            if (child.GetType() == typeof(TerminalNodeImpl))
            {
                IToken? node = ((TerminalNodeImpl) child).Symbol;
                if (childTypes.Contains(node.Type)) currentList.Add(node.Text);
            }
            else
            {
                AllPoints(child, currentList);
            }
        }

        return currentList;
    }

    public static List<string> AllOfType(IParseTree context, int childType, List<string> currentList) =>
        AllOfType(context, new HashSet<int> {childType}, currentList);


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

    public static string HelpText()
    {
        StringBuilder builder = new StringBuilder();

        builder.Append("Siemens-PPCL.exe\n");
        builder.Append("\n");
        builder.Append("USAGE:\n");
        builder.Append("Siemens-PPCL.exe file\n");
        builder.Append("\n");
        builder.Append("Analyzes a file for 'ALMACK' points.\n");
        builder.Append("OPTIONS:\n");
        builder.Append("  -h, --help   display help and exit\n");
        builder.Append("  --gosub INT  GOSUB line to include for strobe points\n");
        builder.Append("  --args ARGS      0 based argument numbers, comma separated, for strobe points on GOSUB\n");
        builder.Append("  --print-filename   Print file name along with points\n");
        builder.Append("  --print-arg-points Print $ARG strobe points - useful for finding files with GOSUB\n");
        builder.Append("  --version          Print version information\n");

        return builder.ToString();
    }
}

public static class Extensions
{
    public static string StripQuotes(this string inputString)
    {
        while (true)
        {
            if (inputString.Length > 1 && inputString[0] == '"' && inputString[^1] == '"')
            {
                inputString = inputString[1..^1];
                continue;
            }

            return inputString;
        }
    }
}
