#!/usr/bin/python

import collections, re

class Production:
	def __init__(self, lhs, rhs, flags=None):
		self.lhs, self.rhs, self.flags = lhs, rhs, flags

class ContextFreeGrammar:
	def __init__(self, productions):
		self.productions = productions
		# === STEP 0
		# Guarantee that we were fed in something in Chomsky normal form.
		assert all(isinstance(prod, Production) for prod in self.productions)

		# === STEP 1
		# Now that our grammar is in (a weak variant of) Chomsky normal form, we categorize the productions.

		# The set of all non-terminal strings. (That is, all the LHSs of productions.)
		self.non_terminals = set(prod.lhs for prod in self.productions)

		# The set of non-terminals that have an epsilon-production.
		self.epsilons = []

		# self.conversions[non_terminal] -> list of right-hand-sides of unary (conversion) productions to other non-terminals that the non-terminal could make.
		# That is, self.conversions[t1] -> [t2, t3, t4, ...] with t1, t2, etc as various non-terminal strings.
		self.conversions = collections.defaultdict(list)

		# self.binary_productions[non_terminal] -> list of right-hand-sides of binary productions that the non_terminal could make.
		# That is, self.binary_productions[t1] -> [[t1, t2], [t3, t4], ...], with t1, t2, etc as various other non-terminal strings.
		self.binary_productions = collections.defaultdict(list)

		# self.terminations[non_terminal] -> list of right-hand-sides of unary productions to terminals(!) that the non-terminal could make
		self.terminations = collections.defaultdict(list)

		for production in self.productions:
			lhs, rhs = production.lhs, production.rhs
			# Be generous in what we accept -- implicitly cast tuples to lists here.
			if isinstance(rhs, tuple):
				rhs = list(rhs)
			if isinstance(rhs, list):
				if len(rhs) == 0:
					self.epsilons.append(production)
				elif len(rhs) == 1:
					self.conversions[lhs].append(production)
				elif len(rhs) == 2:
					self.binary_productions[lhs].append(production)
			else:
				self.terminations[lhs].append(production)

	def parse(self, non_terminal, tokens):
		# Here we use a (slight generalization) of Cocke-Younger-Kasami to parse in $O(n^3)$ time.

		cache = collections.defaultdict(list)
		# Pre-populate the cache with all leaves that match individual terminals.
		for i, token in enumerate(tokens):
			for name, productions in self.terminations.iteritems():
				for production in productions:
					lhs, rhs = production.lhs, production.rhs
					if rhs(token):
						cache[(lhs, i, 1)].append(("toks", production, [token]))
		# Pre-populate each epsilon production.
		for i in xrange(len(tokens)+1):
			for production in self.epsilons:
				cache[(production.lhs, i, 0)].append(("toks", production, []))

		def parse_helper(non_terminal, start, span):
			args = (non_terminal, start, span)
			if args not in cache:
				cache[args] = result = []
				# Try conversions.
				for production in self.conversions[non_terminal]:
					sub_result = parse_helper(production.rhs[0], start, span)
					if sub_result:
						result.append(("->1", production, sub_result))
				# Try matching each production.
				for production in self.binary_productions[non_terminal]:
					for split_point in xrange(span+1):
						a = parse_helper(production.rhs[0], start, split_point)
						# If here to early-out.
						if a:
							b = parse_helper(production.rhs[1], start + split_point, span - split_point)
							if b:
								result.append(("->2", production, a, b))
			return cache[args]

		combinatorial_representation = parse_helper(non_terminal, 0, len(tokens))

		# We now have a compact combinatorial representiation of the potentially exponentially (or even infinitely) many derivations.
		# We define an expansion method that produces a depth-first-search of the tree of derivations that is specified.
		# This does NOT guarantee an enumeration of all possible derivations, if infinitely many are possible.
		# However, such an enumeration could be produced by a slightly more complicated expand function.
		def expand(node):
			for option in node:
				option_kind, production = option[:2]
				if option_kind == "toks":
					yield (production, option[2])
				elif option_kind == "->1":
					for sub_option in expand(option[2]):
						yield (production, sub_option)
				elif option_kind == "->2":
					for sub_option1 in expand(option[2]):
						for sub_option2 in expand(option[3]):
							yield (production, sub_option1, sub_option2)

		for derivation in expand(combinatorial_representation):
			yield derivation
#			yield self.dechomskyify(derivation)

	__call__ = parse

class BNFParser:

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
				self.productions.append(Production(wrapper_non_terminal, closure, {"flatten": True, "rename": None, "drops": []}))
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
#X		# If the blob is suffixed with % then it isn't dropped, but what it matches is flattened into
#X		# the parent's node in the derivation tree. ("flattened production")
		def make_non_terminal_from_blob(blob):
			flags = {"drop": False}
			# First, we check for flags and eliminate them.
			if blob.endswith("~"):
				flags["drop"] = True
				blob = blob[:-1]
#			if blob.endswith("%"):
#				flags["flatten"] = True
#				blob = blob[:-1]

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
			foo = Production(lhs, [node.non_terminal for node in rhs], prod_flags)
			self.productions.append(foo)

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
		# To fix this, we flatten nodes in the derivation tree that begin with "_chain_" or are terminal applications.
		output = []
		stack = [output]
		def helper(node):
			# The first value in every node is a production.
			production = node[0]
			# Check whether or not to build a new entry in our output AST.
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

			# Finally, pop the stack entry if appropriate.
			if not production.flags["flatten"]:
				stack.pop()

#			elif node[0].startswith("_chain_"):
#				assert len(node) == 3
#				map(helper, node[1:])
#			else:
#				new_entry = (node[0], [])
#				stack[-1].append(new_entry)
#				stack.append(new_entry[-1])
#				map(helper, node[1:])
#				stack.pop()
		helper(derivation)
		return output[0]

	def parse(self, root_non_terminal, tokens):
		for derivation in self.grammar(root_non_terminal, tokens):
			return self.dechomskyify(derivation)

	__call__ = parse

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

#if __name__ == "__main__":
if True:
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
literal ::= integer::~ | float::
parens ::= "(" expr ")"
operation ::= expr operator:: expr
expr ::= literal | parens | operation
""")

	tokens, remaining = lex("(3.2+1) / ((2.7))")
	print tokens, repr(remaining)
	assert remaining == "", "Failed to completely lex."

	for derivation in parser("expr", tokens):
		pprint.pprint(derivation)

if __name__ == "__main__" and False:
	import pprint
	lex = Lexer(open("data/lexer.regexes").read())
	parser = BNFParser(open("data/grammar.bnf").read())
	tokens, remaining = lex(r"""\
\x -> x \
""")
	print tokens, repr(remaining)
	assert remaining == ""
	for derivation in parser("syntax_element", tokens):
		pprint.pprint(derivation)

