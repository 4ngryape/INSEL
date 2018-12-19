require 'rbconfig'
require 'yaml'
# Tries to understand on which system this script is running, where Insel is installed
# and what the name of the executable is.
#
# *) if config.yml exists, reads it.
# *) if config.yml doesn't exist, creates an empty one
# *) if inselroot.ini exists, reads it and parses where Insel is installed
# *) if neither inselroot.ini nor config.yml exist, use system default

module Insel
  version         = 8

  default_configs = {
    'linux'   => {'insel_path' => '/opt/insel/resources'                                   , 'insel_command' => './insel'}          ,
    'windows' => {'insel_path' => File.join(ENV['ProgramFiles'] || '','insel 8/resources') , 'insel_command' => 'insel.exe'}      ,
    'mac'     => {'insel_path' => '/opt/insel/Contents'                                    , 'insel_command' => './insel'}        ,
    'wine'    => {'insel_path' => '/opt/insel/resources'                                   , 'insel_command' => 'wine insel.exe'}
  }

  config_file     = File.join(INSEL_RUBY_ROOT, 'config.yml')

  unless File.exists?(config_file)
    File.open(config_file,'w+'){|f|
      f.puts "insel_path      :
insel_command   :"
    }
  end

  system = case RbConfig::CONFIG['host_os']
           when /linux|cygwin/i then "linux"
           when /mac|darwin/i   then "mac"
           when /mswin|mingw/i  then "windows"
           else puts "Which system are you using? Please contact support"
           end

  if system == "windows" then
    ini_file  = File.join(ENV['ALLUSERSPROFILE'] , 'INSEL', 'inselroot.ini')
  else
    ini_file  = "/opt/insel/inselroot.ini"
  end

  if system == "mac" then
    subfolder = 'Contents'
  else
    subfolder = 'resources'
  end

  default_config = default_configs[system]

  if File.exists?(ini_file) then
    ini_content = File.read(ini_file)
    if ini_content =~ /inselroot *= *(.*)/i then
      tmp_path            = $1.strip
      insel_path_from_ini = File.join(tmp_path,subfolder)
      default_config['insel_path'] = insel_path_from_ini
    end
  end

  local_config   = YAML.load(File.open(config_file))
  local_config['insel_extension'] = (version.to_i == 8 ? '.insel' : '.ins')
  Configuration  = default_config.safe_merge(local_config)
end
