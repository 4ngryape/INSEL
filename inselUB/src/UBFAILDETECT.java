package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/")

@BlockInfo(function="ub0094",
   inMin  = 6,
   inMax  = 6,
   inIni  = 3,
   outMin = 2,
   outMax = 2,
   outIni = 2,
   bpMin  = 4,
   bpMax  = 4,
   spMin  = 0,
   group =  3,
   spMax  = 0)

public final class UBFAILDETECT extends Block <UBFAILDETECT>
{
	
	public @StringType(init="3920") Attribute<String> bp1;
	public @StringType(init="3920") Attribute<String> bp2;
	public @StringType(init="3920") Attribute<String> bp3;
	public @StringType(init="3920") Attribute<String> bp4;
	
	

   public UBFAILDETECT(){}
}