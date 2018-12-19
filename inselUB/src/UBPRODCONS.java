package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/home.png")

@BlockInfo(function="ub0012",
   inMin  = 2,
   inMax  = 2,
   inIni  = 2,
   outMin = 3,
   outMax = 3,
   outIni = 3,
   bpMin  = 0,
   bpMax  = 0,
   spMin  = 0,
   group  = 3,
   spMax  = 0)

public final class UBPRODCONS extends Block <UBPRODCONS>
{
   public UBPRODCONS(){}
}

