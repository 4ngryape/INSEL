package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.EnumType;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;


@Icon(path = "icons/electricity_p.png")
@BlockInfo(function = "ub0161",
      group = 3,
      inMin = 8,
      inMax = 8,
      inIni = 8,
      outMin = 10,
      outMax = 10,
      outIni = 10,
      bpMin = 12,
      bpMax = 12,
      spMin = 1,
      spMax = 1)
public final class UBHPREFP extends Block <UBHPREFP>
{
   private static enum Enum {
      R134a, R407c, CO2
   }

   public @StringType(init = "0.8")
   Attribute<String> bp1;
   public @StringType(init = "4.18")
   Attribute<String> bp2;
   public @StringType(init = "1.012")
   Attribute<String> bp3;
   public @StringType(init = "5.8")
   Attribute<String> bp4;
   public @StringType(init = "9.6")
   Attribute<String> bp5;
   public @EnumType(init = "R134a")
   Attribute<Enum> bp6;
   public @StringType(init = "0.4982")
   Attribute<String> bp7;
   public @StringType(init = "0.01744")
   Attribute<String> bp8;
   public @StringType(init = "-0.0002259")
   Attribute<String> bp9;
   public @StringType(init = "-0.008621")
   Attribute<String> bp10;
   public @StringType(init = "-0.0002352")
   Attribute<String> bp11;
   public @StringType(init = "0.0003956")
   Attribute<String> bp12;
   public @StringType(init = "path")
   Attribute<String> sp1;

   public UBHPREFP()
   {
   }
}
