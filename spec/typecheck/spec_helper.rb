#!/usr/bin/env ruby
# -*- coding: utf-8 -*-


module Marked
  CommentStyle = {
    haskell: "--",
    ruby:    "#",
  }

  def self.parse contents, mark, comment_style
    contents.lines.inject([nil, false]) do |(acc, marked), line|
      begin
        text, = /^#{comment_style}\s(.*\n?)/.match(line).captures
        if marked
          [acc + text, marked]
        elsif text.chomp == mark.to_s
          [acc ||= "", true]
        else
          [acc, marked]
        end
      rescue NoMethodError
        [acc, false]
      end
    end.first
  end
end
