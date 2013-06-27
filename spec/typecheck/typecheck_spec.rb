#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "rspec"


options = []
directory = File.expand_path File.dirname __FILE__
if Dir.exists? "cabal-dev"
  options << "-package-db=#{Dir.glob("cabal-dev/packages-*.conf").first}"
end
if Dir.exists? ".cabal-sandbox"
  options << "-package-db=#{Dir.glob(".cabal-sandbox/*-packages.conf.d").first}"
end

describe "typecheck:" do
  Dir.glob("#{directory}/should_compile/*").each do |test|
    it "should succeed to compile #{test}" do
      system("ghc -fno-code #{options.join(" ")} #{test} 1>/dev/null 2>&1").should be_true
    end
  end

  failures = "/home/maksenov/git/biegunka-core/tests/typecheck/should_fail"
  Dir.glob("#{directory}/should_fail/*").each do |test|
    it "should fail to compile #{test}" do
      system("ghc -fno-code #{options.join(" ")} #{test} 1>/dev/null 2>&1").should be_false
    end
  end
end
