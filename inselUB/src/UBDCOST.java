package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/dcost.png")

@BlockInfo(function="ub0102",
   group = 3,
   inMin  = 3,
   inMax  = 10,
   inIni  = 3,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 1,
   bpMax  = 1,
   spMin  = 0,
   spMax  = 0)

public final class UBDCOST extends Block <UBDCOST>
{
	
public @StringType(init="0") Attribute<String> bp1;
		

   public UBDCOST(){}
}