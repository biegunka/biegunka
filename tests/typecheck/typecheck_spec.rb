#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "rspec"

def usage
  <<-USAGE
    Usage: packagedb=$PATH_TO_CABAL_PACKAGES rspec typecheck_spec.rb
  USAGE
end


packagedb = ENV['packagedb'] || (raise usage)
directory = File.expand_path File.dirname __FILE__


describe "typecheck:" do
  Dir.glob("#{directory}/should_compile/*").each do |test|
    it "should succeed to compile #{test}" do
      system("ghc -fno-code -package-db=#{packagedb} #{test} 1>/dev/null 2>&1").should be_true
    end
  end

  failures = "/home/maksenov/git/biegunka-core/tests/typecheck/should_fail"
  Dir.glob("#{directory}/should_fail/*").each do |test|
    it "should fail to compile #{test}" do
      system("ghc -fno-code -package-db=#{packagedb} #{test} 1>/dev/null 2>&1").should be_false
    end
  end
end
