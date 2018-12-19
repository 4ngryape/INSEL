package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/UBDELAY.png")

@BlockInfo(function="ub0095",
   inMin  = 2,
   inMax  = 2,
   inIni  = 2,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 0,
   bpMax  = 0,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBDELAY extends Block <UBDELAY>
{

	

   public UBDELAY(){}
}