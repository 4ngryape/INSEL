package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/pause.png")

@BlockInfo(function="ub0104",
   inMin  = 1,
   inMax  = 4,
   inIni  = 1,
   outMin = 0,
   outMax = 4,
   outIni = 0,
   bpMin  = 0,
   bpMax  = 2,
   spMin  = 1,
   group = 3,
   spMax  = 2)

public final class UBPAUSE extends Block <UBPAUSE>
{
	
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="") Attribute<String> sp1;
	public @StringType(init="") Attribute<String> sp2;



   public UBPAUSE(){}
}