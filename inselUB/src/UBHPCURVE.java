package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.InputFieldLook;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/HPPOLY.png")

@BlockInfo(function="ub0110",
   inMin  = 0,
   inMax  = 20,
   inIni  = 7,
   outMin = 0,
   outMax = 20,
   outIni = 6,
   bpMin  = 1,
   bpMax  = 9999,
   spMin  = 0,
   group = 3,
   spMax  = 0)

public final class UBHPCURVE extends Block <UBHPCURVE>
{
	public @StringType(init="3.8") Attribute<String> bp1;
	public @StringType(init="4.19") Attribute<String> bp2;




   public UBHPCURVE(){}
}