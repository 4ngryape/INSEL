package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/ubfor.png")

@BlockInfo(function="ub0017",
   inMin  = 4,
   inMax  = 4,
   inIni  = 4,
   outMin = 3,
   outMax = 3,
   outIni = 3,
   bpMin  = 2,
   bpMax  = 2,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UB3WAYVALVE extends Block <UB3WAYVALVE>
{
	public @StringType(init="80") Attribute<String> bp1;
	public @StringType(init="4190") Attribute<String> bp2;

   public UB3WAYVALVE(){}
}

