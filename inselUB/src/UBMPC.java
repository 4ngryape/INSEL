package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/reg.png")

@BlockInfo(function="ub0091",
   group = 3,
   inMin  = 0,
   inMax  = 10,
   inIni  = 0,
   outMin = 1,
   outMax = 2,
   outIni = 2,
   bpMin  = 3,
   bpMax  = 10,
   spMin  = 1,
   spMax  = 3)

public final class UBMPC extends Block <UBMPC>
{
	public @StringType(init="0.0") Attribute<String> bp1;
	public @StringType(init="2.0") Attribute<String> bp2;
	public @StringType(init="2.0") Attribute<String> bp3;
	public @StringType(init="0.0") Attribute<String> bp4;
	public @StringType(init="2.0") Attribute<String> bp5;
	public @StringType(init="2.0") Attribute<String> bp6;	
	public @StringType(init="2.0") Attribute<String> bp7;
	public @StringType(init="2.0") Attribute<String> bp8;
	public @StringType(init="") Attribute<String> sp1;
	public @StringType(init="") Attribute<String> sp2;
	public @StringType(init="") Attribute<String> sp3;	

   public UBMPC(){}
}