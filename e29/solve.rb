require 'json'
require 'pry'

puts
puts "start..."

$data_file = "data/data.json"

@event = open($data_file) { |io| JSON.load io }

Problem = Struct.new :number, :src, :expected

@problems = @event['test_data'].map { |obj| Problem.new *Problem.members.map { |k| obj[k.to_s] } }


class StateMonad
  def initialize &proc
    @proc = proc
  end

  def run state
    @proc[state]
  end

  def bind p = nil, &proc
    StateMonad.new do |s|
      x, s_ = self.run s
      (p || proc)[x].run s_
    end
  end

  def then m
    StateMonad.new do |s|
      _, s_ = self.run s
      m.run s_
    end
  end
end


def pure x
  StateMonad.new { |s| [x, s] }
end
def get
  StateMonad.new { |s| [s, s] }
end
def gets
  StateMonad.new { |s| [yield(s), s] }
end
def put x
  StateMonad.new { |_| [nil, x] }
end
def modify
  StateMonad.new { |s| [nil, yield(s)] }
end

def chars
  get.bind { |s|
    str = s.sub(/\A([a-zA-Z0-9]+)/, "")
    x = $1
    x ? put(str).then(pure x) : pure(nil)
  }
end

def double_slash
  get.bind { |s|
    str = s.sub(/\A(\/\/)/, "")
    x = $1
    x ? put(str).then(pure '/') : pure(nil)
  }
end

def plain
  many(
    chars.bind { |x|
      x ? pure(x) : double_slash
    }
  ).bind { |xs| xs.join.empty? ? pure(nil) : pure(xs.join) }
end

def quated q
  q_ = ['\'', '\"'] - [q]
  get.bind { |s|
    xs = []
    str = s.sub(/\A#{q}([#{q_.join}\/a-zA-Z0-9]*?)#{q}/) do |m|
      xs << $1
      ""
    end
    xs.empty? ? pure(nil) : put(str).then(pure xs.join)
  }
end

def slash
  get.bind { |s|
    (s[0] == '/') ? put(s[1..-1]).then(pure '/') : pure(nil)
  }
end

def entry
  many(
    plain.bind { |x|
      x ? pure(x) : quated('\'').bind { |x|
        x ? pure(x) : quated('\"')
      }
    }
  ).bind { |xs| xs.join.empty? ? pure(nil) : pure(xs.join) }
end

def path
  entry.bind { |x|
    many(
      slash.then(entry)
    ).bind { |xs| pure [x, *xs] }
  }
end

def many m
  nexts = ->(xs) {
    get.bind { |s|
      m.bind { |x|
        if x
          nexts[[*xs, x]]
        else
          put(s).then(pure(xs))
        end
      }
    }
  }
  nexts[[]]
end

def solve problem
  ents, rem = path.run(problem.src)
  ans = (rem.empty? && !ents.include?(nil)) ? ents.join(',') : '-'
  if ans == problem.expected
    puts "%2d. OK %s => %s" % [problem.number, problem.src, ans]
  else
    puts "%2d. NG %s => %s" % [problem.number, problem.src, ans]
    puts " " * (problem.src.length + 8) + "?= #{problem.expected}"
  end
end

@problems.each do |p|
  solve p
end
