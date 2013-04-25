#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "rspec"


packagedb = ENV['packagedb']
directory = File.expand_path File.dirname __FILE__


describe "typecheck:" do
  Dir.glob("#{directory}/should_compile/*").each do |test|
    it "should succeed to compile #{test}" do
      system("ghc -fno-code -package-db=#{packagedb} #{test}").should be_true
    end
  end

  failures = "/home/maksenov/git/biegunka-core/tests/typecheck/should_fail"
  Dir.glob("#{directory}/should_fail/*").each do |test|
    it "should fail to compile #{test}" do
      system("ghc -fno-code -package-db=#{packagedb} #{test}").should be_false
    end
  end
end
