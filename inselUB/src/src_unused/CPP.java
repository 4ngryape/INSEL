package eu.insel.userblock;

import de.vseit.network.Attribute;
import de.vseit.network.schema.Icon;
import de.vseit.network.schema.StringType;
import eu.insel.block.Block;
import eu.insel.block.BlockInfo;

@Icon(path="icons/cpp.png")

@BlockInfo(function="ub0001",
		group  = 3,
		inMin=1,
		inMax=1,
		inIni=1,
		outMin=1,
		outMax=1,
		outIni=1,
		bpMin=1,
		bpMax=1,
		spMin=0,
		spMax=0)

public final class CPP extends Block <CPP>
{
	public @StringType(init="1") Attribute<String> bp1;

	public CPP(){}
}
