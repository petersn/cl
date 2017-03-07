#!/usr/bin/python

import collections, re

class ContextFreeGrammar:
	"""ContextFreeGrammar
	__init__(self, productions)

	An implementation of Cocke-Younger-Kasami, parsing sequences of tokens against a Chomsky Normal Form Context Free Grammar.
	The argument `productions' must be a list whose elements are each a ContextFreeGrammar.Production.
	It is perhaps easiest to show by example. The following is a CFG that matches strings of correctly nested parens.

	>>> Production = ContextFreeGrammar.Production
	>>> cfg = ContextFreeGrammar([
	...     Production("balanced_or_nothing", []),
	...     Production("balanced_or_nothing", ["balanced"]),
	...     Production("balanced", ["open", "chain"]),
	...     Production("chain", ["balanced_or_nothing", "close"]),
	...     Production("balanced", ["balanced", "balanced"]),
	...     Production("open", lambda x: x == "("),
	...     Production("close", lambda x: x == ")"),
	... ])
	>>> derivations = list(cfg.parse("balanced_or_nothing", list("()(()())")))
	>>> len(derivations)
	1

	The ContextFreeGrammar.parse(self, non_terminal, tokens) yields all ways of matching tokens against the non-terminal.
	"""

	class Production:
		"""ContextFreeGrammar.Production
		__init__(self, lhs, rhs, flags=None)

		Each production consists of an `lhs', which should be any hashable comparable (string, int) representing a non-terminal,
		and an `rhs', which must be one of the following:
			1) A list of length 0: represents an epsilon production lhs -> \epsilon
			2) A list of length 1: represents the conversion production lhs -> rhs[0]
			3) A list of length 2: represents the production lhs -> rhs[0] rhs[1]
			4) A callable. In this case, the function will be called on terminals (that is, tokens),
			   and must return True when the lhs may be converted into said terminal via this rule.

		Finally, flags is data that is simply carried along for the ride, for your accounting purposes.
		(In particular, BNFParser uses this field mark some data about how the production should be
		expanded in the final AST, but ContextFreeGrammar doesn't know about that. For more info on that,
		see BNFParser's docstring.)
		"""
		def __init__(self, lhs, rhs, flags=None):
			self.lhs, self.rhs, self.flags = lhs, rhs, flags

		def __repr__(self):
			return "<%r ::= %r>" % (self.lhs, self.rhs)

	def __init__(self, productions):
		self.productions = productions
		assert all(isinstance(prod, ContextFreeGrammar.Production) for prod in self.productions), \
			"ContextFreeGrammar wants a list of Productions."

		# The only preprocessing we do is to split up the productions into the four cases:
		#   self.terminations:       non-terminal -> terminal 
		#   self.epsilons:           non-terminal -> (nothing)
		#   self.conversions:        non-terminal -> non-terminal
		#   self.binary_productions: non-terminal -> non-terminal non-terminal
		# The first two structures are lists of all Productions of those kinds.
		# The last two structures are simply dicts mapping a non-terminal foobar to a list
		# of all the Productions (of the given kind) which have foobar as their LHS.
		self.terminations = []
		self.epsilons = []
		self.conversions = collections.defaultdict(list)
		self.binary_productions = collections.defaultdict(list)

		for production in self.productions:
			lhs, rhs = production.lhs, production.rhs
			# Be generous in what we accept as the RHS of a Production -- implicitly cast tuples to lists here.
			if isinstance(rhs, tuple):
				rhs = list(rhs)
			# Test for the first three cases:
			if isinstance(rhs, list):
				if len(rhs) == 0:
					self.epsilons.append(production)
				elif len(rhs) == 1:
					self.conversions[lhs].append(production)
				elif len(rhs) == 2:
					self.binary_productions[lhs].append(production)
			else:
				# If not, then we must be a terminal production, and rhs is the callable that checks for matches.
				assert callable(rhs), "Terminals need to be defined as callables that return True on matching tokens."
				self.terminations.append(production)

	def parse(self, non_terminal, tokens):
		"""parse(self, non_terminal, tokens) -> generator yielding all derivations

		Specifically, `non_terminal' should be the LHS of one or more Productions in the grammar,
		and `tokens' should be a list of tokens that can be matched against with the callables.
		In general, parse takes time that is $O(n^3 |G|)$, with $n$ being the length of `tokens',
		and $|G|$ being the size of the grammar.
		"""

		# The CYK parsing algorithm is basically just dynamic programming, which we implement via memoization.
		# We define that:
		#     memo[non_terminal, start_index, span] = [derivation0, derivation1, ...]
		# Specifically, we want memo[non_terminal, start_index, span] to be the derivations of non_terminal
		# to produce the sequence of tokens given by tokens[start_index:start_index+span].
		# Each derivation is a tuple of one of the following forms:
		#     ("toks", production, [token0, token1, ...])
		#   This form corresponds to literally matching the given span with some sequence of tokens.
		#   Note that if the span is zero the sequence of tokens may be empty.
		#   NB: The current implementation will only ever use zero or one tokens in the sequence.
		#     ("->1", production, [derivation0, derivation1, ...])
		#   This form corresponds to applying a conversion production, that can match via some other list of derivations.
		#     ("->2", production, [derivation0, ...], [derivation0, ...])
		#   This form corresponds to applying a binary production, where the first RHS term can be made in some list of
		#   different ways, and the second RHS term can be made in another list of ways. Their cartesian product yields
		#   the full list of ways that the given production matches the given start index and span.
		# Note that when the grammar would yield infinitely many parsings, this manifests itself as lists including
		# themselves indirectly. This does not change the validity of the memo as an abstract representation of the
		# various ways of matching a given non-terminal against tokens[start_index:start_index+span].
		memo = collections.defaultdict(list)

		# Pre-populate the memo with all tokens that match individual terminals.
		for i, token in enumerate(tokens):
			for production in self.terminations:
				lhs, rhs = production.lhs, production.rhs
				# This is the ONLY place where we call the terminal callables, and match them against tokens.
				if rhs(token):
					# This is a perfect example of our memo's structure:
					# Here we found that `production' with LHS `lhs' matched tokens[i:i+1].
					# Thus, we append ("toks", production, [token]) into memo[lhs, i, 1], marking
					# that this is a valid way of deriving those tokens with the non-terminal `lhs'.
					memo[lhs, i, 1].append(("toks", production, [token]))

		# Pre-populate each epsilon production.
		# Specifically, every non-terminal with an epsilon production matches *every* empty sequence of tokens,
		# so simply add a whole raft of matches into the memo. The sequences we're matching against are effectively
		# of the form tokens[i:i] for every i in [0, 1, ... len(tokens)].
		for i in xrange(len(tokens)+1):
			for production in self.epsilons:
				memo[production.lhs, i, 0].append(("toks", production, []))

		total_calls = [0]
		def dynamic_programming(non_terminal, start, span):
			"""dynamic_programming(non_terminal, start, span) -> derivations of non_terminal over tokens[start:start+span]

			This method is the memoized dynamic programming call.
			It checks if the argument triple is in the memo, and if not, populates that memo entry and returns it.
			"""
			total_calls[0] += 1
			args = (non_terminal, start, span)
			# If our arguments aren't in the memo, then do the work to populate this cell of the memo.
			if args not in memo:
				# For subtle reasons relating to the fact that our memo will have nodes that link
				# to each other (and potentially cyclically), it is CRITICAL that we insert something
				# into the memo right now, because our recursive calls could actually call back to
				# this set of arguments (referencing this memo cell) before we exit this call.
				# This would result in an infinite recursion unless we insert someting here now.
				memo[args] = result = []
				# First we try conversion productions, and see if any of them yield derivations.
				for production in self.conversions[non_terminal]:
					sub_result = dynamic_programming(production.rhs[0], start, span)
					# If we want our memo to be 100% for grammrs with cyclic conversions, then we shouldn't
					# perform this check here, because it could be that the other rule only gets populated
					# once some rule in its cycle successfully gets a derivation via a binary production.
					# There are more complicated ways of handling this, but rather than handle certain
					# grammars that yield infinite derivation trees better, I forgo correctness on them
					# for a performance gain on the finite derivation trees I care about most.
					# TAG: ISSUE1
					if sub_result:
						result.append(("->1", production, sub_result))
				# Next we try out all the binary productions for this non-terminal...
				for production in self.binary_productions[non_terminal]:
					# ... and search across split points.
					for split_point in xrange(span+1):
						# Thus, in here we will try to match production.rhs[0] and production.rhs[1]
						# against tokens[start:start+split_point] and tokens[start+split_point:start+span] respectively.
						a = dynamic_programming(production.rhs[0], start, split_point)
						# If here to early-out.
						if a:
							# Note the computation of the new span as span - split_point.
							b = dynamic_programming(production.rhs[1], start + split_point, span - split_point)
							if b:
								# If we got matches for each half of the RHS, then we yield a match for the binary production.
								result.append(("->2", production, a, b))
			return memo[args]

		# Populate the memo with the outermost computation of our desired non-terminal against the whole input.
		combinatorial_representation = dynamic_programming(non_terminal, 0, len(tokens))

#		print total_calls[0]

		# We now have a compact combinatorial representiation of the potentially exponentially (or
		# even infinitely) many derivations. Modulo the concerns of ISSUE1 above, this combinatorial
		# representation contains all derivations, including all derivations that are infinite trees
		# (with infinitely many epsilons). We define an expansion method that produces a depth-first-
		# search of the tree of derivations that is specified. This does NOT guarantee an enumeration
		# of all possible derivations, if infinitely many are possible. However, such an enumeration
		# could be produced by a slightly more complicated expand function.
		def expand(node):
			for option in node:
				option_kind, production = option[:2]
				if option_kind == "toks":
					yield (production, option[2])
				elif option_kind == "->1":
					for sub_option in expand(option[2]):
						yield (production, sub_option)
				elif option_kind == "->2":
					# Compute the cartesian product of the two lists of derivations.
					for sub_option1 in expand(option[2]):
						for sub_option2 in expand(option[3]):
							yield (production, sub_option1, sub_option2)

		for derivation in expand(combinatorial_representation):
			yield derivation

	__call__ = parse

class BNFParser:
	"""BNFParser
	__init__(self, text)

	Parses according to a grammar specified in a generalized BNF syntax.
	The main method is BNFParser.parse(self, tokens) -> iterable of derivations.
	The expectation is that the input tokens will be produced by a Lexer, whose
	tokens are of the form ("class", "characters"), for some token class "class",
	and some characters "characters".

	The core syntax that the BNFParser accepts is:
		// Comments are like this.
		non_terminal ::= term term... | term term...
	Each term may either be a non-terminal, or a terminal specifier.

	TODO: Flesh out documentation of this.
	For now, see the bottom of this file for examples.
	"""

	# A BNF node stores a non-terminal, and some associated data, and is an object that appears on the RHS of a production.
	# The purpose is to allow non-terminals to be marked up with renaming, dropping, and flattening rules.
	class BNFNode:
		def __init__(self, non_terminal, flags):
			self.non_terminal, self.flags = non_terminal, flags

	def __init__(self, text):
		self.productions = []

		temp_non_terminal_index = [0]
		def temp_non_terminal():
			temp_non_terminal_index[0] += 1
			return temp_non_terminal_index[0]

		# Our underlying CYK implementation in ContextFreeGrammar won't allow terminals to appear in any production
		# other than a simple (non_terminal -> terminal) form, so we have to wrap any terminals in a non-terminal
		# that can be used in its place to implement productions like (non_terminal -> non_terminal terminal)
		# This method make_matcher produces a lambda that matches (lex_class, lex_string) blobs that come out
		# of our Lexer, and adds a production for a newly generated non-terminal to wrap this lambda, then returns
		#the newly generated non-terminal. If either lex_class or lex_string is None, then that field matches any
		# value. The cache makes sure that we don't produce lots of unnecessary equivalent non-terminals for
		# equivalent terminals.
		terminal_wrapper_cache = {}
		def make_matcher(lex_class=None, lex_string=None):
			args = (lex_class, lex_string)
			if args not in terminal_wrapper_cache:
				wrapper_non_terminal = temp_non_terminal()
				# Make a single production for our new non-terminal.
				# This non-terminal can now stand in for the desired terminal.
				closure = lambda (_class, _string): (lex_class is None or _class == lex_class) and (lex_string is None or _string == lex_string)
				closure.func_name = "%s::%s" % (lex_class, lex_string)
				self.productions.append(ContextFreeGrammar.Production(wrapper_non_terminal, closure, {"flatten": True, "rename": None, "drops": []}))
				# Set the cache to say that this terminal spec maps to this wrapper non-terminal.
				terminal_wrapper_cache[args] = wrapper_non_terminal
			return terminal_wrapper_cache[args]

		# This routine takes a specification of a "blob", and produces a non-terminal that matches it.
		# The options are as follows:
		#   "foo" -> matches any lex class, but only the string "foo"
		#   ::"foo" -> equivalent to the above
		#   cls:: -> matches only the lex class cls, any string
		#   cls::"foo" -> matches only the lex class cls, and string "foo"
		#   anythingelse -> ... is interpreted as a non-terminal
		# Additionally, if the blob is suffixed with ~ then it is dropped from the final parsing. ("dropped production")
		def make_non_terminal_from_blob(blob):
			flags = {"drop": False}
			# First, we check for flags and eliminate them.
			if blob.endswith("~"):
				flags["drop"] = True
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
				return self.BNFNode(make_matcher(class_spec, string_spec), flags)
			else:
				# Otherwise, it's a non-terminal and we return it unscathed.
				return self.BNFNode(blob, flags)

		def build_production(lhs, rhs, prod_flags):
			"""build_production(lhs, rhs, prod_flags) -> None
			`prod_flags' should be {"flatten": bool}.
			Adds a production to `productions' corresponding (lhs, rhs) by scanning the BNFNodes in rhs,
			and adding a drop entry to prod_flags for each node with its drop flag set.
			"""
			# Because we mutate this, to be safe, copy it first.
			prod_flags = prod_flags.copy()
			prod_flags["drops"] = []
			# We inspect the various RHS terms of the production.
			for i, node in enumerate(rhs):
				if node.flags["drop"]:
					prod_flags["drops"].append(i)
			self.productions.append(ContextFreeGrammar.Production(lhs, [node.non_terminal for node in rhs], prod_flags))

		# We have a simple BNF-like syntax for specifying our grammar.
		for line in text.split("\n"):
			line = line.split("//")[0].strip()
			if not line:
				continue
			assert "::=" in line, "Malformed BNF line: %r" % line

			# Build rules.
			overall_lhs, overall_rhs = line.split("::=", 1)
			overall_lhs = overall_lhs.strip()

			# We now extract some flags for this rule.
			flags = {
				"flatten": False,
				"rename": None,
			}

			if overall_lhs.endswith("%"):
				overall_lhs = overall_lhs[:-1]
				flags["flatten"] = True
			if ":" in overall_lhs:
				overall_lhs, renaming = overall_lhs.split(":", 1)
				flags["rename"] = renaming

			for rhs_seq in [i.split() for i in overall_rhs.split("|")]:
				# We now convert the sequence of blobs into non-terminals, and insert a production.
				rhs_seq = map(make_non_terminal_from_blob, rhs_seq)

				# Next, we put this sequence into Chomsky normal form.
				# While we have more than one non-terminal, we repeatedly merge the last two into one.
				while len(rhs_seq) > 1:
					# Pop the last two off.
					last_two = rhs_seq.pop(-2), rhs_seq.pop(-1)
					new_non_terminal = temp_non_terminal()
					build_production(new_non_terminal, last_two, {"flatten": True})
					rhs_seq.append(self.BNFNode(new_non_terminal, {"drop": False}))

				# NB: Epsilon productions are handled by this code path too -- rhs_seq might be [].
				build_production(overall_lhs, rhs_seq, flags)

		# TODO: Do conversion pruning (or other optimizations of the grammar) here for efficiency.

		self.grammar = ContextFreeGrammar(self.productions)

	def dechomskyify(self, derivation):
		# Because for CYK we must convert our grammar to Chomsky Normal Form our derivations
		# look really ugly, and in particular, don't match the input form we were given.
		# To fix this, we flatten nodes in the derivation tree that have the "flatten" flag set.
		output = []
		stack = [output]
		def helper(node):
			# The first value in every node is a production.
			production = node[0]
			# === Check whether or not to build a new entry in our output AST ===
			# We build a node precisely when the production doesn't say to "flatten".
			if not production.flags["flatten"]:
				name = production.lhs
				# If name is not None, then let it override the LHS's name as the name for this entry in the AST.
				if production.flags["rename"] != None:
					name = production.flags["rename"]
				# Generate the new AST entry, and write it into the current place in the aST.
				new_entry = (name, [])
				stack[-1].append(new_entry)
				# Finally, push the new_entry's list into the stack so further nodes are built into *it*.
				stack.append(new_entry[-1])

			# === Recursively build the contents of this node ===
			# This is where drops are taken into account.

			# If node[1] is a list, then we've found a terminal application or epsilon production.
			if isinstance(node[1], list):
				# Here we drop iff 0 is in drops, because we have only a single child.
				if 0 not in production.flags["drops"]:
					stack[-1].extend(node[1])
			else:
				for i, subnode in enumerate(node[1:]):
					# Skip subnodes that we are told to drop from the flags.
					if i in production.flags["drops"]:
						continue
					helper(subnode)

			# === Finally, pop the stack entry if appropriate ===
			if not production.flags["flatten"]:
				stack.pop()

		helper(derivation)
		return output[0]

	def parse(self, root_non_terminal, tokens):
		for derivation in self.grammar(root_non_terminal, tokens):
			yield self.dechomskyify(derivation)

	__call__ = parse

class Lexer:
	"""Lexer
	___init__(self, text)

	Use with Lexer.lex(self, string) -> sequence of tokens.

	TODO: Document properly.
	For now, look to the bottom of this file for examples.
	"""
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

def pretty(x, indent=0):
	if isinstance(x[1], str):
		return " "*indent + x[1]
#	if len(x[1]) == 1:
#		return " "*indent + x[0] + ": " + pretty(x[1][0], indent+2)
	return " "*indent + x[0] + "\n" + "\n".join(pretty(i, indent+2) for i in x[1])

if __name__ == "__main__":
	import pprint, sys
	sys.setrecursionlimit(4000)
	lex = Lexer(open("data/lexer.regexes").read())
	parser = BNFParser(open("data/grammar.bnf").read())
	s = " ".join(sys.argv[1:])
	tokens, remaining = lex(s)
	# For temporary testing purposes, we strip out newline tokens, so we
	# can test our parser as though we're matching a single syntax element.
	tokens = [tok for tok in tokens if tok[0] != "newline"]
	print tokens, repr(remaining)
	assert remaining == ""
	for derivation in parser("syntax_element", tokens):
		print pretty(derivation)
#		pprint.pprint(derivation)

