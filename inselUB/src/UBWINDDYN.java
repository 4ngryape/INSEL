package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/window.png")

@BlockInfo(function="ub0033",
   inMin  = 11,
   inMax  = 14,
   inIni  = 11,
   outMin = 9,
   outMax = 9,
   outIni = 9,
   bpMin  = 9,
   bpMax  = 9,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBWINDDYN extends Block <UBWINDDYN>
{
	public @StringType(init="0") Attribute<String> bp1;
	public @StringType(init="0") Attribute<String> bp2;
	public @StringType(init="0") Attribute<String> bp3;
	public @StringType(init="0") Attribute<String> bp4;
	public @StringType(init="0") Attribute<String> bp5;
	public @StringType(init="0") Attribute<String> bp6;
	public @StringType(init="0") Attribute<String> bp7;
	public @StringType(init="0") Attribute<String> bp8;
	public @StringType(init="", inputFieldLook=@InputFieldLook(lines=20)) Attribute<String> bp9;

   public UBWINDDYN(){}
}

