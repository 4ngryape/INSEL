package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/room.png")

@BlockInfo(function="ub0037",
   inMin  = 8,
   inMax  = 11,
   inIni  = 8,
   outMin = 3,
   outMax = 6,
   outIni = 3,
   bpMin  = 9,
   bpMax  = 9,
   group  = 3,
   spMin  = 0,
   spMax  = 0)

public final class UBROOMLIGHT2015 extends Block <UBROOMLIGHT2015>
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

   public UBROOMLIGHT2015(){}
}

