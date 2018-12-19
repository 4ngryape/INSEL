package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/cube.png")

@BlockInfo(function="ub0090",
   inMin  = 1,
   inMax  = 4,
   inIni  = 1,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 1,
   bpMax  = 1,
   spMin  = 0,
   group = 3,
   spMax  = 1)

public final class UBRANDNUMB extends Block <UBRANDNUMB>
{
	public @StringType(init="1") Attribute<String> bp1;


   public UBRANDNUMB(){}
}