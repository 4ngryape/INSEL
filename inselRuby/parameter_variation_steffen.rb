

# Needed library in order to call Insel blocks and templates from Ruby
# Loads the content of lib/insel.rb
require File.expand_path("lib/insel",File.dirname(__FILE__))

# Avoids writing 'Insel::Block' instead of just 'Block'
include Insel

min_diff = Float::INFINITY
goal = 0
best_parameters = nil
# Calculates 5*7 with a template
#   Template.template_name(:variable1 => value1, ..., :variableN => valueN)
(0.1..0.9).step(0.1).each do |thickness|
	(0.5..3).step(0.1).each do |conductivity|
		(100..2000).step(100).each do |density|
			(100..1500).step(100).each do |cp|
				print "+"
				parameters = {:thickness => thickness, :conductivity => conductivity, :density => density, :cp => cp}
				results = Template.steffen_test(parameters)
				result = results.last
				if (result-goal).abs min_diff then
					min_diff =(result-goal).abs
					best_parameters = parameters
					puts
				    puts parameters.inspect
				    puts "\t#{result}"
				end
			end
		end
	end
end

puts "Best parameters :"
puts min_diff
puts best_parameters.inspect
