package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/write.png")

@BlockInfo(function="ub0105",
   inMin  = 0,
   inMax  = 20,
   inIni  = 0,
   outMin = 0,
   outMax = 20,
   outIni = 1,
   bpMin  = 2,
   bpMax  = 9,
   spMin  = 1,
   group = 3,
   spMax  = 1)

public final class UBWRITE extends Block <UBWRITE>
{
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="") Attribute<String> sp1;



   public UBWRITE(){}
}