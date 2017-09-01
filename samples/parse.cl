# Generic CFG parser implemented in Cl.

def all iterable
	for i <- iterable
		if i
		else
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
	if x
	else
		traceback("Assert failure.")
	end
end

def isinstance a b
	if getkind(a) != "Instance"
		return False
	end
	return getparent(a) == b
end

class DefaultDict
	def construct factory
		
	end
end

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

d = {}
d[1] = 2
print d[1]

exit()

p = ContextFreeGrammar.Production(1, 2, 3)
print isinstance(p, ContextFreeGrammar)
cfg = ContextFreeGrammar([p])

