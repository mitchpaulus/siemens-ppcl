using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;

namespace Siemens_PPCL;

internal class AntlrErrorListener : IAntlrErrorListener<int>, IAntlrErrorListener<IToken>
{
    public List<string> Errors = new();

    public void SyntaxError(TextWriter output, IRecognizer recognizer, int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
    {
        var message = $"{line}:{charPositionInLine}: {msg}\n";
        Errors.Add(message);
    }

    public void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
    {
        var message = $"{line}:{charPositionInLine}: {msg}\n";
        Errors.Add(message);
    }
}