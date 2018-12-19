
# Needed library in order to call Insel blocks and templates from Ruby
# Loads the content of lib/insel.rb
require File.expand_path("lib/insel",File.dirname(__FILE__))

# Avoids writing 'Insel::Block' instead of just 'Block'
include Insel

data_path = 'C:\Users\Marcus.Brennenstuhl\Documents\insel.work\read'

min_diff = Float::INFINITY
goal = 0
best_parameters = nil
# Calculates 5*7 with a template
#   Template.template_name(:variable1 => value1, ..., :variableN => valueN)
(1..3).step(1).each do |x|
	(1..3).step(1).each do |y|
								print "+"
								parameters = {:x => x, :y => y}
								results = Template.fritz(parameters.merge(:path => data_path))
								result = results.last
#								if (results-goal).abs < min_diff then
#								min_diff =(results-goal).abs
#								best_parameters = parameters
								puts results.inspect
#								puts parameters.inspect
#								puts "\t#{results}"
#								end

	end
end

puts "Best parameters :"
puts min_diff
puts results.inspect
