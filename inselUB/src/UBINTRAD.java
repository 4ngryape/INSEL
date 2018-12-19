package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/radi.png")

@BlockInfo(function="ub0034",
   inMin  = 4,
   inMax  = 20,
   inIni  = 4,
   outMin = 4,
   outMax = 20,
   outIni = 4,
   bpMin  = 3,
   bpMax  = 3,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBINTRAD extends Block <UBINTRAD>
{
	public @StringType(init="6") Attribute<String> bp1;
	public @StringType(init="", inputFieldLook=@InputFieldLook(lines=20)) Attribute<String> bp2;
	public @StringType(init="", inputFieldLook=@InputFieldLook(lines=20)) Attribute<String> bp3;

   public UBINTRAD(){}
}

