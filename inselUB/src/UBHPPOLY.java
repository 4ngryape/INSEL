package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/HPPOLY.png")

@BlockInfo(function="ub0164",
   inMin  = 1,
   inMax  = 4,
   inIni  = 4,
   outMin = 1,
   outMax = 9,
   outIni = 9,
   bpMin  = 25,
   bpMax  = 25,
   spMin  = 0,
group = 1,
   spMax  = 0)

public final class UBHPPOLY extends Block <UBHPPOLY>
{
	public @StringType(init="1") Attribute<String> bp1;
	public @StringType(init="4.18") Attribute<String> bp2;
	public @StringType(init="3.8") Attribute<String> bp3;
	public @StringType(init="138.164833") Attribute<String> bp4;
	public @StringType(init="206.335159") Attribute<String> bp5;
	public @StringType(init="1.76e+04") Attribute<String> bp6;
	public @StringType(init="506.9") Attribute<String> bp7;
	public @StringType(init="-82.06") Attribute<String> bp8;
	public @StringType(init="4.642") Attribute<String> bp9;
	public @StringType(init="-1.016") Attribute<String> bp10;
	public @StringType(init="-0.6289") Attribute<String> bp11;
	public @StringType(init="0.01329") Attribute<String> bp12;
	public @StringType(init="-0.01279") Attribute<String> bp13;
	public @StringType(init="-0.03278") Attribute<String> bp14;
	public @StringType(init="-0.004119") Attribute<String> bp15;
	public @StringType(init="1652") Attribute<String> bp16;
	public @StringType(init="-4.228") Attribute<String> bp17;
	public @StringType(init="43.45") Attribute<String> bp18;
	public @StringType(init="0.6478") Attribute<String> bp19;
	public @StringType(init="0.3524") Attribute<String> bp20;
	public @StringType(init="-0.01564") Attribute<String> bp21;
	public @StringType(init="-0.008698") Attribute<String> bp22;
	public @StringType(init="-0.0125") Attribute<String> bp23;
	public @StringType(init="-0.002671") Attribute<String> bp24;
	public @StringType(init="0.01046") Attribute<String> bp25;
	

   public UBHPPOLY(){}
}