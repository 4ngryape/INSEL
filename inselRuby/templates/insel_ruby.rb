# Needed library in order to call Insel blocks and templates from Ruby
# Loads the content of lib/insel.rb
require File.expand_path("lib/insel",File.dirname(__FILE__))

# Avoids writing 'Insel::Block' instead of just 'Block'
include Insel

# Returns the value of Pi block
#   Block.block_name
puts Block.pi

# Calculates sin((6+4)*9) = sin (90) = 1
#   Block.block_name(input1,input2,....,inputN)
puts Block.sin(Block.mul(Block.sum(6,4), 9))

# Creates an Array from 1 to 10
#   Block.launch(:block_name, [parameter1,parameter2,...,parameterM])
puts Block.launch(:do, [1,10,1]).inspect

# Gets average temperature in december in Strasbourg [°C]
#   Block.new(:block_name, [parameter1,parameter2,...,parameterM],input1, input2, ...., inputN)[which_output]
puts Block.new(:mtm,['Strasbourg'], 12)[2]

# Calculates 5*7 with a template
#   Template.template_name(:variable1 => value1, ..., :variableN => valueN)
puts Template.a_times_b(:a => 5, :b => 7)

# Fill factor in % of SunPower SPR-305-WHT-I by STC [%]
# NOTE: The pv_id could be different on other systems
puts Template.fill_factor(:pv_id => '003281', :temperature=> 25, :irradiance => 1000)*100

# Isc of SunPower SPR-305-WHT-I by STC [A]
puts Template.i_sc(:pv_id => '003281', :temperature=> 25, :irradiance => 1000)

# Defaults values can be specified inside the template :
#           $ irradiance || 1000 $
#           $ temperature || 25 $
puts Template.i_sc(:pv_id => '003281')
puts Template.i_sc(:pv_id => '003281', :temperature => 75)

# Uoc of SunPower SPR-305-WHT-I by STC [A]
puts Template.u_oc(:pv_id => '003281')

# Example of "parametric analyses" :
[1,2,3,4,5,6,7,8,9,10].each{|e| puts Insel::Template.a_times_b(:a=> e,:b=>5)}

(-25..75).step(25){|ta| puts Template.fill_factor(:pv_id=> '003281',:temperature => ta)}

# It is also possible to use Arrays as variable.
#  s 2 CONST
#  p 2
#           $ x[0] $
#  s 3 CONST
#  p 3
#           $ x[1] $
# No default values are allowed with this syntax
puts Template.x1_plus_x2(:x => [4,5])

# Arrays could be useful to provide irradiance or temperature profiles
name       = 'Roma'
lat        = 41.8  
lon        = -12.58
timezone   = 23
irradiance = Template.get_irradiance_profile(:latitude => lat, :insel_longitude => lon)
# Monthly irradiances [W/m2] as Array
puts irradiance.inspect

puts Template.average_irradiance_on_tilted_surface(
  :tilt               => 30,
  :azimuth            => 180,
  :irradiance_profile => irradiance,
  :latitude           => lat,
  :insel_longitude    => lon,
  :insel_timezone     => timezone
)
