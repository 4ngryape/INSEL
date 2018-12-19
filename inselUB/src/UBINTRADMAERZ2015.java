package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/ubfor.png")

@BlockInfo(function="ub0039",
   inMin  = 6,
   inMax  = 20,
   inIni  = 6,
   outMin = 6,
   outMax = 20,
   outIni = 6,
   bpMin  = 7,
   bpMax  = 7,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UBINTRADMAERZ2015 extends Block <UBINTRADMAERZ2015>
{
	public @StringType(init="10") Attribute<String> bp1;
	public @StringType(init="3") Attribute<String> bp2;
	public @StringType(init="50") Attribute<String> bp3;
	public @StringType(init="4") Attribute<String> bp4;
	public @StringType(init="0") Attribute<String> bp5;
	public @StringType(init="", inputFieldLook=@InputFieldLook(lines=10)) Attribute<String> bp6;
	public @StringType(init="", inputFieldLook=@InputFieldLook(lines=10)) Attribute<String> bp7;

   public UBINTRADMAERZ2015(){}
}

