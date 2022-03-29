using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Siemens_PPCL;

internal class PPCLListener : PPCLBaseListener
{
    public List<PPCLParser.LineContext> lines = new();

    public override void EnterLine(PPCLParser.LineContext context)
    {
        lines.Add(context);
    }
}