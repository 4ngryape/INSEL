package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/load.png")

@BlockInfo(function="ub0025",
   group = 3,
   inMin  = 7,
   inMax  = 7,
   inIni  = 7,
   outMin = 1,
   outMax = 7,
   outIni = 1,
   bpMin  = 2,
   bpMax  = 2,
   spMin  = 0,
   spMax  = 0)

public final class UBH0LP extends Block <UBH0LP>
{
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;

   public UBH0LP(){}
}

