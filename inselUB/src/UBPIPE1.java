package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/")

@BlockInfo(function="ub0015",
   inMin  = 4,
   inMax  = 4,
   inIni  = 4,
   outMin = 2,
   outMax = 2,
   outIni = 2,
   bpMin  = 6,
   bpMax  = 6,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UBPIPE1 extends Block <UBPIPE1>
{
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="0") Attribute<String> bp3;
	public @StringType(init="0") Attribute<String> bp4;
	public @StringType(init="0") Attribute<String> bp5;
	public @StringType(init="0") Attribute<String> bp6;
	

   public UBPIPE1(){}
}