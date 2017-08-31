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

def isinstance a b
	if getkind(a) != "Instance"
		return False
	end
	return getparent(a) == b
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
		@.conversions = {"a": 1, "b": 2}
		@.binary_productions = {}
	end
end

p = ContextFreeGrammar.Production(1, 2, 3)
print isinstance(p, ContextFreeGrammar)
cfg = ContextFreeGrammar(p)

