package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/for.png")

@BlockInfo(function="ub0053",
   inMin  = 3,
   inMax  = 3,
   inIni  = 3,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 2,
   bpMax  = 2,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UB2PCOOLCON2 extends Block <UB2PCOOLCON2>
{
	public @StringType(init="5.0") Attribute<String> bp1;
	public @StringType(init="0.001") Attribute<String> bp2;
	

   public UB2PCOOLCON2(){}
}