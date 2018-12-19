package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/array.png")

@BlockInfo(function="ub0107",
   inMin  = 0,
   inMax  = 20,
   inIni  = 1,
   outMin = 0,
   outMax = 20,
   outIni = 1,
   bpMin  = 1,
   bpMax  = 900000,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBARRAY extends Block <UBARRAY>
{
	public @StringType(init="", inputFieldLook=@InputFieldLook(lines=900000)) Attribute<String> bp1;




   public UBARRAY(){}
}