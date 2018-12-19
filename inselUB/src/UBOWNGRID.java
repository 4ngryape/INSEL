package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/Grid.png")

@BlockInfo(function="ub0014",
   inMin  = 5,
   inMax  = 5,
   inIni  = 5,
   outMin = 3,
   outMax = 5,
   outIni = 3,
   bpMin  = 0,
   bpMax  = 0,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UBOWNGRID extends Block <UBOWNGRID>
{
   public UBOWNGRID(){}
}

