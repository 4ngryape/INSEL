package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/")

@BlockInfo(function="ub0112",
   inMin  = 0,
   inMax  = 0,
   inIni  = 0,
   outMin = 1,
   outMax = 1,
   outIni = 1,
   bpMin  = 2,
   bpMax  = 2,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBCOUNT extends Block <UBCOUNT>

{
	public @StringType(init="1") Attribute<String> bp1;
	public @StringType(init="1") Attribute<String> bp2;


   public UBCOUNT(){}
}