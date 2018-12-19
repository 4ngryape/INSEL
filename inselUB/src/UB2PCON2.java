package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/for.png")

@BlockInfo(function="ub0052",
   group = 3,
   inMin  = 2,
   inMax  = 2,
   inIni  = 2,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 3,
   bpMax  = 3,
   spMin  = 0,
   spMax  = 0)

public final class UB2PCON2 extends Block <UB2PCON2>
{
	public @StringType(init="35.0") Attribute<String> bp1;
	public @StringType(init="5.0") Attribute<String> bp2;
	public @StringType(init="0.001") Attribute<String> bp3;
	

   public UB2PCON2(){}
}