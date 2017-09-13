# Generic CFG parser implemented in Cl.

def enumerate iterable
	l = []; i = 0
	for x <- iterable
		l.append([i, x])
		i += 1
	end
	return l
end

def all iterable
	for i <- iterable
		if not i
			return False
		end
	end
	return True
end

def any iterable
	for i <- iterable
		if i
			return True
		end
	end
	return False
end

def assert x
	if not x
		traceback("Assert failure.")
	end
end

def isinstance a b
	if getkind(a) != "Instance"
		return False
	end
	return getparent(a) == b
end

class Slice
	def construct start stop
		@.start, @.stop = [start, stop]
	end

	def __rindex__ other
		l = []
		for i <- upto(@.stop - @.start)
			l.append(other[@.start + i])
		end
		return l
	end
end

def join sep l
	out = ""
	for i, s <- enumerate(l)
		out += s
		if i != len(l) - 1
			out += sep
		end
	end
	return out
end

l = [x.str() | x <- [1, 2, 3, 4, 5]]
print l
print join(", ", l)

l = [1,2,3,4,5]
#print l[Slice(1, 2)]

class DefaultDict
	def construct
		@.contents = {}
	end

	def str
		return "{" + join(", ", [k.str() + ": " + v.str | k, v <- @.contents]) + "}"
	end

	def __index__ other
		if not other in @.contents
			@.contents[other] = []
		end
		return @.contents[other]
	end

	def __set_index__ key value
		@.contents[key] = value
	end
end

d = DefaultDict()
d["a"] = 1
print d["b"]
print d.str()

exit()

class ContextFreeGrammar
	class Production
		def construct lhs rhs flags
			@.lhs, @.rhs, @.flags = [lhs, rhs, flags]
		end

		def str
			return "<" + @.lhs.str() + " ::= " + @.rhs.str() + ">"
		end

		def iter
			return ([1, 2, 3]).iter()
		end
#		iter = ([1, 2, 3]).iter
	end

	def construct productions
		@.productions = productions
		assert(all([isinstance(prod, ContextFreeGrammar.Production) | prod <- productions]))
		@.terminations = []
		@.epsilons = []
		@.conversions = {}
		@.binary_productions = {}

		for production <- @.productions
			lhs, rhs = [production.lhs, production.rhs]
			if getkind(rhs) == "list"
				if len(rhs) == 0
					@.epsilons.append(production)
				elif len(rhs) == 1
					@.conversions[lhs].append(production)
				elif len(rhs) == 2
					@.binary_productions[lhs].append(production)
				end
			end
		end
	end

	def parse non_terminal tokens
	end
end

p = ContextFreeGrammar.Production(1, 2, 3)
print isinstance(p, ContextFreeGrammar)
cfg = ContextFreeGrammar([p])

