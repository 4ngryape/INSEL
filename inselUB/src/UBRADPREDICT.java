package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/ubfor.png")

@BlockInfo(function="ub092",
   inMin  = 1,
   inMax  = 15,
   inIni  = 1,
   outMin = 1,
   outMax = 15,
   outIni = 1,
   bpMin  = 0,
   bpMax  = 0,
   spMin  = 3,
   group  = 3,
   spMax  = 3)

public final class UBRADPREDICT extends Block <UBRADPREDICT>
{
	public @StringType(init="") Attribute<String> sp1;
	public @StringType(init="") Attribute<String> sp2;
	public @StringType(init="") Attribute<String> sp3;	

   public UBRADPREDICT(){}
}

