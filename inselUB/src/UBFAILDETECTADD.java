package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/")

@BlockInfo(function="ub0093",
   inMin  = 2,
   inMax  = 2,
   inIni  = 2,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 1,
   bpMax  = 1,
   spMin  = 0,
   group =  3,
   spMax  = 0)

public final class UBFAILDETECTADD extends Block <UBFAILDETECTADD>
{
	
	public @StringType(init="0") Attribute<String> bp1;

	

   public UBFAILDETECTADD(){}
}