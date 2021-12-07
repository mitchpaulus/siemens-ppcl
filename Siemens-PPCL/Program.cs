using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
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

            CommonTokenStream tokens = new CommonTokenStream(lexer);

            PPCLParser parser = new PPCLParser(tokens);

            PPCLParser.ProgramContext? tree = parser.program();

            GotoListener listener = new GotoListener();

            ParseTreeWalker walker = new ParseTreeWalker();
            walker.Walk(listener, tree);

            Console.Write(listener.builder.ToString());

            Console.Write("\nUnique Lines:\n");

            List<int> sortedLines = listener.GotoLocations
                .Select(int.Parse)
                .OrderBy(i => i).ToList();

            foreach (int line in sortedLines)
            {
                Console.Write($"{line}\n");
            }
        }
    }
}
