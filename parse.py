#!/usr/bin/python

import collections, re

class ContextFreeGrammar:
	def __init__(self, productions):
		self.productions = []

		# === STEP 1
		# The first thing we do is regularlize the productions into a variant of Chomsky normal form.
		# This allows Cocke-Younger-Kasami to be implemented efficiently.
		index = [0]
		def new_nonterminal():
			index[0] += 1
			return "_chain_%i" % index[0]

		for lhs, rhs in productions:
			# First, automagically convert tuples to lists.
			if isinstance(rhs, tuple):
				rhs = list(rhs)
			# If the production is to zero or more non-terminals, then make sure it isn't to more than two.
			if isinstance(rhs, list) and len(rhs) > 2:
				symbol = rhs[-1]
				for i in range(len(rhs)-1)[::-1]:
					# TODO: Cache these values?
					# If we've seen (rhs[i], symbol) before we don't need to generate a new non-terminal.
					new_symbol = new_nonterminal() if i > 0 else lhs 
					self.productions.append((new_symbol, [rhs[i], symbol]))
					symbol = new_symbol
			elif isinstance(rhs, str):
				# We replace strings with the point function that matches just that string.
				# Note the second lambda (and immediate call) to generate a scope so the closure of rhs works properly.
				self.productions.append((lhs, (lambda rhs: lambda s: s == rhs)(rhs)))
			else:
				self.productions.append((lhs, rhs))

		# === STEP 2
		# Now that our grammar is in (a weak variant of) Chomsky normal form, we categorize the productions.

		# The set of all non-terminal strings. (That is, all the LHSs of productions.)
		self.non_terminals = set(prod[0] for prod in self.productions)

		# The set of non-terminals that have an epsilon-production.
		self.epsilons = set() 

		# self.conversions[non_terminal] -> list of right-hand-sides of unary (conversion) productions to other non-terminals that the non-terminal could make.
		# That is, self.conversions[t1] -> [t2, t3, t4, ...] with t1, t2, etc as various non-terminal strings.
		self.conversions = collections.defaultdict(list)

		# self.binary_productions[non_terminal] -> list of right-hand-sides of binary productions that the non_terminal could make.
		# That is, self.binary_productions[t1] -> [[t1, t2], [t3, t4], ...], with t1, t2, etc as various other non-terminal strings.
		self.binary_productions = collections.defaultdict(list)

		# self.terminations[non_terminal] -> list of right-hand-sides of unary productions to terminals(!) that the non-terminal could make
		self.terminations = collections.defaultdict(list)

		for lhs, rhs in self.productions:
			if isinstance(rhs, list):
				if len(rhs) == 0:
					self.epsilons.add(lhs)
				elif len(rhs) == 1:
					self.conversions[lhs].append(rhs[0])
				elif len(rhs) == 2:
					self.binary_productions[lhs].append(rhs)
			else:
				self.terminations[lhs].append(rhs)

	def dechomskyify(self, derivation):
		# Because for CYK we must convert our grammar to Chomsky Normal Form our derivations
		# look really ugly, and in particular, don't match the input form we were given.
		# To fix this, we flatten nodes in the derivation tree that begin with "_chain_" or are terminal applications.
		output = []
		stack = [output]
		def helper(node):
			# If node[1] is a list, then we've found a terminal application.
			if isinstance(node[1], list):
				stack[-1].extend(node[1])
			elif node[0].startswith("_chain_"):
				assert len(node) == 3
				map(helper, node[1:])
			else:
				new_entry = (node[0], [])
				stack[-1].append(new_entry)
				stack.append(new_entry[-1])
				map(helper, node[1:])
				stack.pop()
		helper(derivation)
		return output[0]

	def parse(self, non_terminal, tokens):
		# Here we use a (slight generalization) of Cocke-Younger-Kasami to parse in $O(n^3)$ time.

		cache = collections.defaultdict(list)
		# Pre-populate the cache with all leaves that match individual terminals.
		for i, token in enumerate(tokens):
			for lhs, rhses in self.terminations.iteritems():
				for rhs in rhses:
					if rhs(token):
						cache[(lhs, i, 1)].append(("toks", lhs, [token]))
		# Pre-populate each epsilon production.
		for i in xrange(len(tokens)+1):
			for epsilon in self.epsilons:
				cache[(epsilon, i, 0)].append(("toks", lhs, []))

		def parse_helper(non_terminal, start, span):
			args = (non_terminal, start, span)
			if args not in cache:
				cache[args] = result = []
				# Try conversions.
				for new_non_terminal in self.conversions[non_terminal]:
					sub_result = parse_helper(new_non_terminal, start, span)
					if sub_result:
						result.append(("->1", non_terminal, sub_result))
				# Try matching each production.
				for rhs in self.binary_productions[non_terminal]:
					for split_point in xrange(span+1):
						a = parse_helper(rhs[0], start, split_point)
						# If here to early-out.
						if a:
							b = parse_helper(rhs[1], start + split_point, span - split_point)
							if b:
								result.append(("->2", non_terminal, a, b))
			return cache[args]

		combinatorial_representation = parse_helper(non_terminal, 0, len(tokens))

		# We now have a compact combinatorial representiation of the potentially exponentially (or even infinitely) many derivations.
		# We define an expansion method that produces a depth-first-search of the tree of derivations that is specified.
		# This does NOT guarantee an enumeration of all possible derivations, if infinitely many are possible.
		# However, such an enumeration could be produced by a slightly more complicated expand function.
		def expand(node):
			for option in node:
				option_kind, non_terminal = option[:2]
				if option_kind == "toks":
					yield (non_terminal, option[2])
				elif option_kind == "->1":
					for sub_option in expand(option[2]):
						yield (non_terminal, sub_option)
				elif option_kind == "->2":
					for sub_option1 in expand(option[2]):
						for sub_option2 in expand(option[3]):
							yield (non_terminal, sub_option1, sub_option2)

		for derivation in expand(combinatorial_representation):
			yield self.dechomskyify(derivation)

	__call__ = parse

class BNFParser:
	def __init__(self, text):
		self.productions = []

		# Our underlying CYK implementation in ContextFreeGrammar won't allow terminals to appear in any production
		# other than a simple (non_terminal -> terminal) form, so we have to wrap any terminals in a non-terminal
		# that can be used in its place to implement productions like (non_terminal -> non_terminal terminal)
		# This method make_matcher produces a lambda that matches (lex_class, lex_string) blobs that come out
		# of our Lexer, and adds a production for a newly generated non-terminal to wrap this lambda, then returns
		#the newly generated non-terminal. If either lex_class or lex_string is None, then that field matches any
		# value. The cache makes sure that we don't produce lots of unnecessary equivalent non-terminals for
		# equivalent terminals.
		terminal_wrapper_index = [0]
		terminal_wrapper_cache = {}
		def make_matcher(lex_class=None, lex_string=None):
			args = (lex_class, lex_string)
			if args not in terminal_wrapper_cache:
				terminal_wrapper_index[0] += 1
				wrapper_non_terminal = "_wrap_%i" % terminal_wrapper_index[0]
				# Make a single production for our new non-terminal.
				# This non-terminal can now stand in for the desired terminal.
				closure = lambda (_class, _string): (lex_class is None or _class == lex_class) and (lex_string is None or _string == lex_string)
				self.productions.append((wrapper_non_terminal, closure))
				# Set the cache to say that this terminal spec maps to this wrapper non-terminal.
				terminal_wrapper_cache[args] = wrapper_non_terminal
			return terminal_wrapper_cache[args]

		self.dropped_productions = set()
		self.flattened_prodcutions = set()

		# This routine takes a specification of a "blob", and produces a non-terminal that matches it.
		# The options are as follows:
		#   "foo" -> matches any lex class, but only the string "foo"
		#   ::"foo" -> equivalent to the above
		#   cls:: -> matches only the lex class cls, any string
		#   cls::"foo" -> matches only the lex class cls, and string "foo"
		#   anythingelse -> ... is interpreted as a non-terminal
		# Additionally, if the blob is suffixed with ~ then it is dropped from the final parsing. ("dropped production")
		# If the blob is suffixed with % then it isn't dropped, but what it matches is flattened into
		# the parent's node in the derivation tree. ("flattened production")
		def make_non_terminal_from_blob(blob):
			dropped = flattened = False
			# First, we check for flags and eliminate them.
			if blob.endswith("~"):
				dropped = True
				blob = blob[:-1]
			if blob.endswith("%"):
				flattened = True
				blob = blob[:-1]

			# As a convenience syntax, "foo" is synonymous with ::"foo"
			if blob.startswith('"') and blob.endswith('"'):
				blob = "::" + blob

			# If the blob has :: in it, then it's a terminal
			if "::" in blob:
				class_spec, string_spec = blob.split("::", 1)
				assert string_spec == "" or (string_spec.startswith('"') and string_spec.endswith('"')), "Bad string spec: %r" % (blob,)
				class_spec = class_spec or None
				string_spec = string_spec[1:-1] or None
				return make_matcher(class_spec, string_spec)
			else:
				# Otherwise, it's a non-terminal and we return it unscathed.
				return blob

		# We have a simple BNF-like syntax for specifying our grammar.
		for line in text.split("\n"):
			line = line.split("//")[0].strip()
			if not line:
				continue
			assert "::=" in line, "Malformed BNF line: %r" % line

			# Build rules.
			lhs, rhs = line.split("::=", 1)
			lhs = lhs.strip()

			for rhs_term in [i.split() for i in rhs.split("|")]:
				# We now convert the sequence of blobs into a non-terminals, and insert a production.
				rhs_term = map(make_non_terminal_from_blob, rhs_term)
				self.productions.append((lhs, rhs_term))

		# TODO: Do conversion pruning (or other optimizations of the grammar) here for efficiency.

		self.grammar = ContextFreeGrammar(self.productions)

		__import__("pprint").pprint(self.productions)
		self.__call__ = self.grammar.__call__

class Lexer:
	def __init__(self, text):
		self.rules = []
		for line in text.split("\n"):
			line = line.split("//")[0].strip()
			if not line:
				continue
			name, regex = line.split("=", 1)
			self.rules.append((name.strip(), re.compile(regex.strip())))

	def lex(self, s):
		tokens = []
		while s:
			for name, regex in self.rules:
				match = regex.match(s)
				if match:
					# Tokens whose name ends with ~ are automatically dropped.
					matched_length = match.end()
					if not name.endswith("~"):
						tokens.append((name, s[:matched_length]))
					# TODO: Replace this with string views, so this doesn't take $O(n^2)$ time...
					s = s[matched_length:]
					break
			else:
				# If nothing matched, then we're done.
				# This will cause us to return what we managed to lex, and the unlexable remainder.
				break
		return tokens, s

	__call__ = lex			

if __name__ == "__main__":
	import pprint

	lex = Lexer("""
// Simple lexer.
open_paren    = [(]
close_paren   = [)]
operator      = [+]|[*]|-|/
float         = [1-9]?[0-9]*[.][0-9]*
integer       = [0-9]+
// Recall that the ~ after whitespace makes it be dropped from output.
whitespace~   = [ ]+|\\t+|\\n+
""")

	parser = BNFParser("""
// Simple parser.
literal ::= integer:: | float::
parens ::= "(" expr ")"
operation ::= expr operator:: expr
expr ::= literal | parens | operation
""")

	tokens, remaining = lex("(3.2+1) / ((2.7))")
	print tokens, repr(remaining)
	assert remaining == "", "Failed to completely lex."

	for derivation in parser("expr", tokens):
		pprint.pprint(derivation)

