package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/wallx.png")

@BlockInfo(function="ub0031",
   inMin  = 10,
   inMax  = 13,
   inIni  = 10,
   outMin = 2,
   outMax = 6,
   outIni = 2,
   bpMin  = 8,
   bpMax  = 8,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBWALLE extends Block <UBWALLE>
{
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="0") Attribute<String> bp3;
	public @StringType(init="0") Attribute<String> bp4;
	public @StringType(init="0") Attribute<String> bp5;
	public @StringType(init="0") Attribute<String> bp6;
	public @StringType(init="0") Attribute<String> bp7;
	public @StringType(init="",inputFieldLook=@InputFieldLook(lines=40)) Attribute<String> bp8;

   public UBWALLE(){}
}

