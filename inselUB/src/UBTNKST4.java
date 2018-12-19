package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/tank.png")

@BlockInfo(function="ub0005",
   group  = 5,
   inMin  = 10,
   inMax  = 10,
   inIni  = 10,
   outMin = 5,
   outMax = 201,
   outIni = 10,
   bpMin  = 12,
   bpMax  = 12,
   spMin  = 0,
   spMax  = 0)

public final class UBTNKST4 extends Block <UBTNKST4>
{
	public @StringType(init="0.75") Attribute<String> bp1;
	public @StringType(init="8") Attribute<String> bp2;
	public @StringType(init="0.7") Attribute<String> bp3;
	public @StringType(init="4190.0") Attribute<String> bp4;
	public @StringType(init="1000.0") Attribute<String> bp5;
	public @StringType(init="0.5") Attribute<String> bp6;
	public @StringType(init="1.0") Attribute<String> bp7;
	public @StringType(init="20.0") Attribute<String> bp8;
	public @StringType(init="20") Attribute<String> bp9;
	public @StringType(init="20") Attribute<String> bp10;
	public @StringType(init="20") Attribute<String> bp11;
	public @StringType(init="8") Attribute<String> bp12;
	
   public UBTNKST4(){}
}

