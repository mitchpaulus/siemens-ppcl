using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Siemens_PPCL;

class GotoListener : PPCLBaseListener
{
    private Stack<string> conditions = new Stack<string>();

    public StringBuilder builder = new StringBuilder();

    private string CurrentMode = "Start";

    public HashSet<string> GotoLocations = new HashSet<string>();


    public override void EnterIfStatement(PPCLParser.IfStatementContext context)
    {
        conditions.Push(context.expression().GetText());
        base.EnterIfStatement(context);
    }

    public override void ExitIfStatement([NotNull] PPCLParser.IfStatementContext context)
    {
        conditions.Pop();
    }

    public override void EnterGotoStatement(PPCLParser.GotoStatementContext context)
    {

        string gotoLine = context.POS_INT().GetText();

        if (conditions.Any())
        {
            builder.Append($"{conditions.Peek()}: {gotoLine}\n");
        }
        else
        {
            builder.Append($"GOTO {gotoLine}"  + "\n");
        }

        GotoLocations.Add(gotoLine);
    }
}