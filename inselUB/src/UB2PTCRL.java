package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/ubfor.png")

@BlockInfo(function="ub0016",
   inMin  = 2,
   inMax  = 2,
   inIni  = 2,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 3,
   bpMax  = 3,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UB2PTCRL extends Block <UB2PTCRL>
{
	public @StringType(init="3") Attribute<String> bp1;
	public @StringType(init="10") Attribute<String> bp2;
	public @StringType(init="95") Attribute<String> bp3;

   public UB2PTCRL(){}
}

