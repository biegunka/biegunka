#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require "open3"
require "rspec"
require_relative "./spec_helper"


def compile file, opts
  external_command = "cabal exec ghc -- -fno-code #{opts.join(" ")} #{file}"
  stdout, stderr, status = Open3.capture3 external_command
  { stdout: stdout, stderr: stderr, exitcode: status.exitstatus }
end

def successes root
  Dir.glob "#{root}/should_compile/*"
end

def failures root
  Dir.glob "#{root}/should_fail/*"
end


options = []
here = File.expand_path File.dirname __FILE__

describe "typechecking," do
  context "when successful," do
    successes(here).each do |success|
      it "succeeds to compile #{success}" do
        process = compile(success, options)
        expect(process[:exitcode]).to eq(0)
        expect(process[:stderr]).to be_empty
      end
    end
  end

  context "when unsuccessful," do
    failures(here).each do |failure|
      it "fails to compile #{failure}" do
        process = compile(failure, options)
        expect(process[:exitcode]).not_to eq(0)

        contents = IO.read(failure)

        marked_stderr = Marked.parse contents, :STDERR, Marked::CommentStyle[:haskell]
        expect(process[:stderr]).to match(/#{marked_stderr.split("\n").join(".+")}/m)
      end
    end
  end
end
