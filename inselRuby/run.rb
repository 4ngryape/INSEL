# Needed library in order to call Insel blocks and templates from Ruby
# Loads the content of lib/insel.rb
require File.expand_path("lib/insel",File.dirname(__FILE__))

# Avoids writing 'Insel::Block' instead of just 'Block'
include Insel



Template.case2

[1,10,100].each{|e| puts Template.case2(:VTank=> e)}