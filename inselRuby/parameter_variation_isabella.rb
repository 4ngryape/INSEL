
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
(0.2..0.9).step(0.1).each do |thickness|
	(0.4..3).step(0.2).each do |conductivity|
		(500..2000).step(100).each do |density|
			(500..1500).step(100).each do |cp|
				(0.2..0.9).step(0.1).each do |thickness2|
					(0.4..3).step(0.2).each do |conductivity2|
						(500..2000).step(100).each do |density2|
							(500..1500).step(100).each do |cp2|
								print "+"
								parameters = {:thickness => thickness, :conductivity => conductivity, :density => density, :cp => cp, :thickness2 => thickness2, :conductivity2 => conductivity2, :density2 => density2, :cp2 => cp2}
								results = Template.isabella_test(parameters.merge(:path => data_path))
#								result = results.last
								if (results-goal).abs < min_diff then
								min_diff =(results-goal).abs
								best_parameters = parameters
								puts
								puts parameters.inspect
								puts "\t#{results}"
								end
							end
						end
					end
				end
			end
		end
	end
end

puts "Best parameters :"
puts min_diff
puts best_parameters.inspect
