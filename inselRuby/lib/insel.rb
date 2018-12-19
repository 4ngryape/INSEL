INSEL_RUBY_ROOT = File.expand_path('../', File.dirname(__FILE__))
require File.join(File.dirname(__FILE__), 'core_exts')
require File.join(File.dirname(__FILE__), 'load_configuration')
require 'fileutils'
require 'tmpdir'

module Insel
  ## This class launches insel with a temporary file containing insel_content
  ## It parses insel output, and returns the results as Float, Array or Array of Arrays
  ## insel_content needs to be defined separately, either with a Block or with a Template
  class Model
    # Parses insel output and return the results.
    # Results are supposed to be between "Running insel" and "Normal end of run"
    # A single value gets returned as Float
    # Multiple lines with single value get returned as an Array
    # One line with multiple values get returned as an Array
    # Multiple lines with multiple values get returned as an Array of Arrays
    def results
      rr = raw_results
      if rr =~/Running insel [\d\w \.]+ \.\.\.\s+([^\*]*)Normal end of run/im then
        $1.split(/\n/).map{|line|
          floats = line.split(/\s+/).reject{|f|f.empty?}.map{|r| r.to_f}
          floats.extract_if_singleton
        }.extract_if_singleton
      else
        raise "problem with INSEL #{rr}"
      end
    end

    # Returns the r-th output
    def [](r)
      @outputs_number=r+1
      results[r]
    end

    private
   
    # Writes a temporary .insel file with insel_content
    # Runs insel
    # Returns the raw output coming from insel
    # Deletes the temporary .insel file
    def raw_results
      temp_file = File.join(Dir.tmpdir, "insel_ruby_#{rand(1000)}.#{Configuration['insel_extension']}")
      FileUtils.cd(Configuration['insel_path']){
        File.open(temp_file, 'w+'){|f|
          f.write insel_content
        }
        @raw_results=%x(#{Configuration['insel_command']} "#{temp_file}")
        FileUtils.rm temp_file
      }
      @raw_results
    end
  end
 

  ## This class is not exactly an insel Block, but an insel Model with one interesting block
  ## and the needed CONST blocks for input and SCREEN block for output.
  ## The main job of this class is to define insel_content. For example, for Block.sum(6,4) :
  #     s 1 CONST
  #     p 1
  #             6
  #     s 2 CONST
  #     p 2
  #             4
  #     s 3 sum 1.1 2.1 
  #     s 4 SCREEN 3.1
  #     p 4
  #             '(6E15.7)'

  class Block < Model
    attr_reader :name, :parameters, :inputs
    
    def initialize(name, parameters, *inputs)
      @name, @parameters, @inputs = name, parameters, inputs
      @outputs_number=1
    end

    # Method to access results from a block with :
    #   Block.launch(:do, [1,10,1]).inspect
    def self.launch(name, parameters, *inputs)
      new(name, parameters, *inputs).results
    end

    # Shortcut to access results from a block without parameters :
    #   Block.sum(6,4)
    def self.method_missing(sim, *inputs)
      launch(sim, [], *inputs)
    end
    
    private
   
    # Defines the model that will be fed to insel
    # Writes the needed CONST blocks, then the interesting block, then SCREEN block
    def insel_content
      tmp_content=[constants, s_part, p_part , screen].compact.join("\n")
      tmp_content.gsub(/i '(.*?)'/){File.read($1)}
    end
   
    # Writes a CONST block for every input
    def constants
      @i=0
      @c_ids = []
      inputs.map{|input|
        @c_ids << "#{@i+=1}.1 "
        "s #{@i} CONST\np #{@i}\n\t#{input}"
      }
    end
   
    # Defines the links between the block and its inputs
    def s_part
      "s #{@i+=1} #{@name} #{@c_ids.join(' ')}"
    end
   
    # Defines the screen block to show the output
    # The outputs_number is 1 by default, but can be defined to be more :
    #   Block.new(:mtm,['Strasbourg'], 12)[2]
    def screen
      input_ids = (1..@outputs_number).map{|o|
        "#{@i}.#{o}"
      }.join(" ")
      "s #{@i+1} SCREEN #{input_ids}\np #{@i+1}\n\t'(6E15.7)'"     
    end
    
    # Writes the parameters for the block, if needed
    def p_part
      ps = parameters.map{|p|
        case p
          when String then "'#{p}'"
          else   p
          end
      }
      ["p #{@i}", ps].join("\n\t") unless parameters.empty?
    end
  end
 
  # Reads a template file present in 'templates' folder with template_name.insel name
  # Replaces every placeholder with specified values and uses it as insel_content
  #
  # For example, templates/a_times_b.insel :
  ########################
  #   s 1 MUL  3.1 2.1
  #   s 2 CONST
  #   p 2
  #              $a$
  #   s 3 CONST
  #   p 3
  #              $b$
  #   s 4 SCREEN  1.1
  #   p 4
  #       '*'
  #
  #######################
  #
  # 
  # Template.a_times_b(:a=> 5, :b=>3)
  # => 15.0
 
  class Template < Model
    attr_reader :name, :parameters, :filename
    
    def initialize(name, parameters={})
      @name, @parameters = name.to_s, parameters.merge(:bp_folder => File.join(Configuration['insel_path'], 'data', 'bp'))
      @filename = File.expand_path(File.join(File.dirname(__FILE__), '..', 'templates', @name+'.insel' ))
    end

    def self.method_missing(sim, *parameters)
      new(sim, *parameters).results
    end
    
    private

    # Replaces every placeholder with specified values and uses it as insel_content
    # Syntax inside .insel template file :
    #   $temperature$
    # or
    #   $ temperature $
    # or
    #   $ temperature || 25 $
    def insel_content
      tmp_content=File.read(@filename)
      tmp_content.gsub!(/\$([\w ]+)(?:\[(\d+)\] *)?(?:\|\|([\w \.]*))?\$/){
        variable_name = $1.strip.to_sym
        index         = $2
        default       = $3
        (index && parameters[variable_name] && parameters[variable_name][index.to_i].to_s) || parameters[variable_name] || default || raise("UndefinedValue for #{variable_name} in #{name}")
      }
      tmp_content.gsub(/i '(.*?)'/){File.read($1)}
    end
  end
end
