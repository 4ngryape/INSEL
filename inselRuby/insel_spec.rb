#encoding: utf-8
# Tests insel blocks and templates
# Can be run with :
#  rspec --format documentation --color insel_spec.rb
require File.expand_path("lib/insel",File.dirname(__FILE__))

describe Insel do
  it "should be installed" do
    File.exists?(Insel::Configuration["insel_path"]).should be_true
  end
  
  it "should be launchable" do
    FileUtils.cd(Insel::Configuration["insel_path"]){
      %x(#{Insel::Configuration["insel_command"]}).should include("This is insel")
    }
  end
end

describe "MUL" do
  it "should have at least one input" do
    lambda{Insel::Block.mul}.should raise_error
  end
  
  it "should not accept any parameter" do
    lambda{Insel::Block.launch(:mul, [5], 4, 3)}.should raise_error
  end
  
  it "should return its input if unique" do
    Insel::Block.mul(3.14).should == 3.14
  end
  
  it "should return the product of 2 numbers" do
    Insel::Block.mul(6,5).should       == 30
    Insel::Block.mul(6,-4).should      == -24
    Insel::Block.mul(-11,-9).should    == 99
    Insel::Block.mul(11.5,3.14).should == 36.11
  end
  
  it "should return the product of 3 numbers or more" do
    Insel::Block.mul(3,5,7).should         == 105
    Insel::Block.mul(*(1..20).to_a).should be_within(1).of(2.432902*10**18)
  end
  
  it "should not multiply Strings" do
    lambda{Insel::Block.mul('Test', 'Not')}.should raise_error
  end
end

describe "SUM" do
  it "should have at least one input" do
    lambda{Insel::Block.sum}.should raise_error
  end
  
  it "should not accept any parameter" do
    lambda{Insel::Block.launch(:sum, [5], 4, 3)}.should raise_error
  end
  
  it "should return its input if unique" do
    Insel::Block.sum(5).should == 5
  end
  
  it "should return the sum of 2 numbers" do
    Insel::Block.sum(6,4).should  == 10
    Insel::Block.sum(-6,4).should == -2
  end
  
  it "should return the sum of 3 numbers or more" do
    Insel::Block.sum(1,2,3).should          == 6
    Insel::Block.sum(*(1..100).to_a).should == 5050
  end
  
  it "should not add Strings" do
    lambda{Insel::Block.sum('Test', 'Not')}.should raise_error
  end
end

describe "GAIN" do
  it "should have exactly one parameter" do
    lambda{Insel::Block.launch(:gain, [], 4)}.should            raise_error
    lambda{Insel::Block.launch(:gain, [1,3], 4)}.should         raise_error
    lambda{Insel::Block.launch(:gain, ['Stuttgart'], 4)}.should raise_error
  end
  
  it "should have at least one input" do
    lambda{Insel::Block.launch(:gain, [3])}.should raise_error
  end
  
  it "should apply gain to input" do
    Insel::Block.launch(:gain, [5], 4).should      == 20
    Insel::Block.launch(:gain, [0.33], 3).should   == 0.99
    Insel::Block.launch(:gain, [-3], 12).should    == -36
    Insel::Block.launch(:gain, [20_000], 4).should == 80_000
  end
  
  it "should apply gain to multiple inputs" do
    lambda{Insel::Block.launch(:gain, [5], 4,6)}.should_not raise_error
    Insel::Block.launch(:gain, [5], 4,6).results == [20,30]
  end
end

describe "ATT" do
  it "should have exactly one parameter" do
    lambda{Insel::Block.launch(:att, [], 4)}.should        raise_error
    lambda{Insel::Block.launch(:att, [4,5], 4)}.should     raise_error
    lambda{Insel::Block.launch(:att, ['Paris'], 4)}.should raise_error
  end
  
  it "hould have at least one input" do
    lambda{Insel::Block.launch(:att, [3])}.should raise_error
  end
  
  it "should apply 1/gain to input" do
    Insel::Block.launch(:att, [5], 4).should      == 0.8
    Insel::Block.launch(:att, [0.33], 3).should   be_within(0.001).of(9.09)
    Insel::Block.launch(:att, [-3], 12).should    == -4
    Insel::Block.launch(:att, [20_000], 4).should == 0.0002
    Insel::Block.launch(:att, [3.14159265], 3.14159265).should == 1
  end
  
  it "should apply 1/gain to multiple inputs" do
    lambda{Insel::Block.launch(:att, [5], 4,6)}.should_not raise_error
    Insel::Block.launch(:att, [5], 4,6).results == [0.8,1.2]
  end
end

describe "PI" do
  it "should be close to 3.14159" do
    Insel::Block.pi.should be_within(1.0e-06).of(Math::PI)
  end
end

describe "MTM" do
  it "should give average temperatures for a given location" do
    Insel::Block.new(:mtm,['Strasbourg'], 12)[2].should == 1.5 # 1.5°C in Strabourg in December
  end
end

describe "Simple template" do
  it "shoud count from one to ten" do
    Insel::Template.one_to_ten.should == (1..10).to_a
  end
end
