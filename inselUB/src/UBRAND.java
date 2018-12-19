package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/cube.png")

@BlockInfo(function="ub0097",
   inMin  = 1,
   inMax  = 4,
   inIni  = 2,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 0,
   bpMax  = 0,
   spMin  = 1,
   group = 3,
   spMax  = 1)

public final class UBRAND extends Block <UBRAND>
{
	public @StringType(init="") Attribute<String> sp1;


   public UBRAND(){}
}