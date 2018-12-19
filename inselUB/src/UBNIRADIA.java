package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/radi.png")

@BlockInfo(function="ub0029",
   inMin  = 1,
   inMax  = 1,
   inIni  = 1,
   outMin = 1,
   outMax = 20,
   outIni = 1,
   bpMin  = 2,
   bpMax  = 2,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBNIRADIA extends Block <UBNIRADIA>
{
	public @StringType(init="1") Attribute<String> bp1;
	public @StringType(init="",inputFieldLook=@InputFieldLook(lines=40)) Attribute<String> bp2;

   public UBNIRADIA(){}
}

