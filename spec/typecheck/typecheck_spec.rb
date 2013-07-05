#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "rspec"


def compile file, opts
  system("ghc -fno-code #{opts.join(" ")} #{file} 1>/dev/null 2>&1")
end

def successes root
  Dir.glob "#{root}/should_compile/*"
end

def failures root
  Dir.glob "#{root}/should_fail/*"
end


options = []
here = File.expand_path File.dirname __FILE__
if File.exists? "cabal-dev"
  options << "-package-db=#{Dir.glob("cabal-dev/packages-*.conf").first}"
elsif File.exists? ".cabal-sandbox"
  options << "-package-db=#{Dir.glob(".cabal-sandbox/*-packages.conf.d").first}"
end

describe "typechecking," do
  context "when successful," do
    successes(here).each do |success|
      it "succeeds to compile #{success}" do
        compile(success, options).should be_true
      end
    end
  end

  context "when unsuccessful," do
    failures(here).each do |failure|
      it "fails to compile #{failure}" do
        compile(failure, options).should be_false
      end
    end
  end
end
