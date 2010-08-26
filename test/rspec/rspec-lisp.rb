#!/usr/bin/env ruby -rubygems
#
# Simple Ruby interface for evaluating Common Lisp code, for use with RSpec.
# Written in August 2010 by Arto Bendiken <http://ar.to/>
# 
# This is free and unencumbered software released into the public domain.

require 'open4' # `sudo gem install open4`
require 'bert'
require 'bert/decode'
require 'bertrpc'

module RSpec; module Lisp
  # SBCL = %q(sbcl --noinform --noprint --no-userinit --disable-debugger --disable-ldb --lose-on-corruption)
  SBCL = %q(sbcl --core sbcl-rspec.core --noinform --noprint --no-userinit --eval "(cl-user::rspec-repl)" --disable-debugger --disable-ldb --lose-on-corruption)
  LISP = SBCL

  class Proxy
    include BERTRPC::Encodes
    attr_accessor :module_name # @return [Symbol]

    ##
    # @param  [Symbol] module_name
    def initialize(module_name = nil, &block)
      @module_name = module_name
      block.call(self) if block_given?
    end

    ##
    # @private
    def method_missing(method_name, *args, &block)
      function_name = method_name.to_s.gsub('_', '-').to_sym
      result = call_lisp_(function_name, args);
      if block_given?
        block.call(result)
      else
        result
      end
    end
  
  ##
  # Proxy a method through BERT-RPC to a lisp process
  #
  # Accept two arguments, function and call arguments, encode them in a call tuple.
  # Establish a connection to a LISP process and dispatch the message its stdin.
  # Accept the result and decode it as per BERTRPC spec, which either yields
  # the result argument list or raises an error.
  # Should the exchangeitself fail, also signal an error.
  # 
  # @param [String] function
  # @param [Array] arguments
    def call_lisp(function, *arguments)
      call_lisp_(function, arguments)
    end


    def call_lisp_(function, arguments)
      # the BERTRPC interface is not symmetric, but that's the way it is.
      message = encode_ruby_request(BERT::Tuple[:call, @module_name, function, arguments])
      pid, stdin, stdout, stderr = Open4.popen4(LISP)
      stdin.write(message)
      stdin.flush.close
      _, status = Process.waitpid2(pid)
      if ( status.exitstatus.zero? )
      # decode the response directly from the stream w/o a length header
        dc = BERT::Decode.new(stdout);
        response = dc.read_any
        case response[0]
          when :reply
            response[1]
          when :error
            error(response[1])
          else
            raise
        end
      else
        raise(StandardError, stderr.read())
      end
    end

  end

  ##
  # Evaluates the given Lisp `expr` string, returning its result as a Ruby
  # value when possible.
  #
  # Boolean, integer, and float return values are currently marshalled into
  # Ruby values.
  #
  # @param  [String] expr
  # @return [Object]
  def self.evaluate(expr)
    case output = execute(%Q((format *standard-output* "~S" #{expr})))
      when 'T'                    then true
      when 'NIL'                  then nil
      when /^[+-]?(?:\d*)?\.\d*$/ then Float(output)
      when /^[+-]?\d+$/           then Integer(output)
      when /^".*"$/               then output[1...-1]
      else output
    end
  end

  ##
  # Executes a given string of Lisp `code`, returning either the standard
  # output or standard error of the Lisp process depending on its exit
  # status.
  #
  # @param  [String] code
  # @return [String]
  def self.execute(code)
    pid, stdin, stdout, stderr = Open4.popen4(LISP)
    stdin.puts(code.to_s)
    stdin.flush.close
    _, status = Process.waitpid2(pid)
    status.exitstatus.zero? ? stdout.read : stderr.read
  end
end; end

if __FILE__ == $0
  p RSpec::Lisp.evaluate(%q("Hello, world!")) #=> "Hello, world!"
  p RSpec::Lisp.evaluate(%q((* 6 (+ 3 4))))   #=> 42
end

## RSpec::Lisp::Proxy.new("CL").call_lisp("lisp-implementation-type")
## RSpec::Lisp::Proxy.new("CL").call_lisp("round", 7, 3)
## RSpec::Lisp::Proxy.new("keyword").call_lisp("rspec.succeed")
## RSpec::Lisp::Proxy.new("keyword").call_lisp("rspec.fail")
## RSpec::Lisp::Proxy.new("keyword").call_lisp("rspec.error")

## RSpec::Lisp::Proxy.new("CL").lisp_implementation_type

