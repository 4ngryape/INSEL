package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/Battery.png")

@BlockInfo(function="ub0024",
   group = 3,
   inMin  = 2,
   inMax  = 2,
   inIni  = 2,
   outMin = 7,
   outMax = 7,
   outIni = 7,
   bpMin  = 9,
   bpMax  = 9,
   spMin  = 0,
   spMax  = 0)

public final class UBSTORAGE extends Block <UBSTORAGE>
{
	public @StringType(init="20") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="0.94") Attribute<String> bp3;
	public @StringType(init="0.94") Attribute<String> bp4;
	public @StringType(init="0") Attribute<String> bp5;
	public @StringType(init="100") Attribute<String> bp6;
	public @StringType(init="0.02") Attribute<String> bp7;
	public @StringType(init="1") Attribute<String> bp8;
	public @StringType(init="0") Attribute<String> bp9;

   public UBSTORAGE(){}
}

