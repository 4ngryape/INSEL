package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/ubfor.png")

@BlockInfo(function="ub0013",
   inMin  = 31,
   inMax  = 31,
   inIni  = 31,
   outMin = 32,
   outMax = 32,
   outIni = 32,
   bpMin  = 5,
   bpMax  = 5,
   spMin  = 3,
   group  = 3,
   spMax  = 3)

public final class UBNETWORK100P extends Block <UBNETWORK100P>
{
	public @StringType(init="5") Attribute<String> bp1;
	public @StringType(init="4") Attribute<String> bp2;
	public @StringType(init="20") Attribute<String> bp3;
	public @StringType(init="50") Attribute<String> bp4;
	public @StringType(init="1E-6") Attribute<String> bp5;
	public @StringType(init="") Attribute<String> sp1;
	public @StringType(init="") Attribute<String> sp2;
	public @StringType(init="") Attribute<String> sp3;

   public UBNETWORK100P(){}
}

