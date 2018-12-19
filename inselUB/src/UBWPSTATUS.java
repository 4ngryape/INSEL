package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/")

@BlockInfo(function="ub0109",
   inMin  = 0,
   inMax  = 20,
   inIni  = 3,
   outMin = 0,
   outMax = 20,
   outIni = 1,
   bpMin  = 5,
   bpMax  = 9,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBWPSTATUS extends Block <UBWPSTATUS>
{
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="0") Attribute<String> bp3;
	public @StringType(init="0") Attribute<String> bp4;
	public @StringType(init="0") Attribute<String> bp5;
	public @StringType(init="0") Attribute<String> bp6;




   public UBWPSTATUS(){}
}