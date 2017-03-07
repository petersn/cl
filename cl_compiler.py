#!/usr/bin/python
# Python based Cl byte code compiler.
# This is a reference-only implementation in Python, before I launch into reimplementing this in C++.

import pprint
import parse
import assemble

class Matcher:
	class DropAny:
		pass

	@staticmethod
	def match_with(tree, pattern):
		"""match_with(tree, pattern) -> pulled out groups
		It is often useful to be able to pull data out of a tree while simultaneously asserting the structure of that tree.
		Here match_with provides this functionality.
		"""
		if isinstance(pattern, (tuple, list)):
			assert type(pattern) == type(tree), "match_with type mismatch: %r against %r" % (tree, pattern)
			assert len(pattern) == len(tree), "match_with shape mismatch: %r against %r" % (tree, pattern)
			output = []
			for i, j in zip(tree, pattern):
				output.extend(Matcher.match_with(i, j))
			return output
		elif isinstance(pattern, (int, str)):
			assert pattern == tree, "match_with value mismatch: %r against %r" % (tree, pattern)
			return []
		elif isinstance(pattern, type):
			assert isinstance(tree, pattern), "match_with type mismatch: %r against %r" % (tree, pattern)
			return [tree]
		elif isinstance(pattern, Matcher.DropAny):
			# Wildcards match everything.
			return []
		raise ValueError("Bad pattern:", pattern)

class SyntaxElement:
	syntax_element_takes_block = set([
		"function_definition",
		"if",
		"while",
		"for",
	])

	def __init__(self, kind, ast):
		self.kind = kind
		self.ast = ast
		self.block = [] if self.takes_block() else None

	def takes_block(self):
		return self.kind in self.syntax_element_takes_block

	@staticmethod
	def pprint_ast(ast, indentation=0):
		if isinstance(ast[1], list):
			print " "*indentation + ast[0]
			for obj in ast[1]:
				SyntaxElement.pprint_ast(obj, indentation+2)
		else:
			print " "*indentation + ast[0], ast[1]

	def pprint(self, indentation=0):
		print " "*indentation + "SYNTAX ELEM:",
		SyntaxElement.pprint_ast(self.ast, indentation=indentation)
		if self.block != None:
			print " "*indentation + "{"
			for obj in self.block:
				obj.pprint(indentation+4)
			print " "*indentation + "}"

class ClParser:
	def __init__(self):
		# Load up our lexer and grammar files.
		with open("data/lexer.regexes") as f:
			self.lexer = parse.Lexer(f.read())
		with open("data/grammar.bnf") as f:
			self.bnf_parser = parse.BNFParser(f.read())

	@staticmethod
	def detokenize(tokens):
		return " ".join(token[1] for token in tokens)

	def parse(self, text):
		# Begin by lexing the input text, and crashing if we couldn't lex it all.
		tokens, remaining = self.lexer(text)
		if remaining:
			print "Lexing error at:", repr(remaining[:40])
			exit(1)

		# Split tokens by newline into syntactic elements.
		syntax_elements = [[]]
		for t in tokens:
			if t[0] == "newline":
				# If the current syntax element is empty, then don't bother splitting into another!
				if syntax_elements[-1] == []:
					continue
				syntax_elements.append([])
			else:
				syntax_elements[-1].append(t)
		# Strip the potential trailing empty syntax element.
		if syntax_elements[-1] == []:
			syntax_elements.pop(-1)

		# Build a derivation for each syntax element.
		derived_syntax_elements = []
		for syntax_element in syntax_elements:
			derivations = list(self.bnf_parser("syntax_element", syntax_element))
			if len(derivations) == 0:
				print "Syntax error on:", self.detokenize(syntax_element)
				exit(1)
			elif len(derivations) > 1:
				print "BUG BUG BUG: Ambiguous syntax on:", self.detokenize(syntax_element)
				exit(1)
			# Extract the unique derivation of this syntax element.
			derivation = derivations[0]
			# This becomes a derived syntax element.
			derived_syntax_elements.append(derivation)

		# We now assemble the syntax elements into an AST, and in particular, enforce various block nesting rules at this point.
		output = []
		stack = [output]
		for derivation in derived_syntax_elements:
			# This derivation will always be of the form ("syntax_element", [(kind of syntax element, [...])])
			# Let's pull out this kind, while simultaneously sanity checking the structure.
			main_element_kind, main_element_contents = Matcher.match_with(derivation,
				("syntax_element", [(str, list)])
			)
			# Build a wrapped SyntaxElement object...
			se = SyntaxElement(main_element_kind, (main_element_kind, main_element_contents))
			# ... and insert it at the appropriate place in the tree.
			if se.kind != "end":
				stack[-1].append(se)

			# If the syntax element wants a block, then we complexify our tree.
			if se.takes_block():
				stack.append(se.block)
			
			# However, if the syntax element is an "end", then we simplify the tree.
			elif se.kind == "end":
				if len(stack) > 1:
					stack.pop()
				else:
					print "Misplaced end, with no block to end."
					exit(1)

		# Finally, we demand that the stack be simply [output] at the end, otherwise an end was mismatched.
		if len(stack) > 1:
			print "Unended block(s)."
			exit(1)
		assert stack == [output]

		return output

class ClCompiler:
	class CompilationContext:
		def __init__(self, target, variable_table, indent=0):
			self.target, self.variable_table = target, variable_table
			self.indent = indent

		def load(self, var):
			self.append("LOAD %i" % self.variable_table.index(var))

		def store(self, var):
			self.append("STORE %i" % self.variable_table.index(var))

		def append(self, x):
			self.target.append(" " * self.indent + x)

	@staticmethod
	def flatten(l):
		"""flatten(nested lists) -> recursively flattened list"""
		out = []
		for entry in l:
			if isinstance(entry, list):
				out.extend(ClCompiler.flatten(entry))
			else:
				out.append(entry)
		return out

	def compute_free_variables(self, ast):
		free = set()
		# For lists, union over entries.
		if isinstance(ast, list):
			for entry in ast:
				free |= self.compute_free_variables(entry)
			return free

		if isinstance(ast, SyntaxElement):
			node_type = ast.ast[0]
			if node_type == "expr":
				subexpression, = Matcher.match_with(ast.ast, ("expr", [tuple]))
				return self.compute_free_variables(subexpression)
			elif node_type == "assignment":
				variable_name, expr = Matcher.match_with(ast.ast,
					("assignment", [
						("identifier", str),
						tuple,
					]))
				return set([variable_name]) | self.compute_free_variables(expr)
			elif node_type == "function_definition":
				# First we compute the free variables in the body of the function...
				body_free = self.compute_free_variables(ast.block)
				# We now pull out the function name, and sequence of identifiers.
				function_name, args_seq = Matcher.match_with(ast.ast,
					("function_definition", [
						("identifier", str),
						("ident_seq", list),
					]))
				args_variables = self.compute_free_variables(args_seq)
				return set([function_name])
			raise ValueError("Unhandled syntax element type: %r" % (ast.ast,))

		# At this point we are guaranteed to be a tuple from the raw AST given by our parser.
		node_type, = Matcher.match_with(ast, (str, Matcher.DropAny()))

		if node_type in ["literal"]:
			return self.compute_free_variables(ast[1])
		elif node_type == "identifier":
			return set([ast[1]])
		elif node_type == "lambda":
			variable_name, expr = Matcher.match_with(ast,
				("lambda", [
					("identifier", str),
					tuple,
				]))
			# We return the free variables in the function body, *minus* the bound variable.
			return self.compute_free_variables(expr) - set([variable_name])
		elif node_type == "binary":
			expr1, operation_class, operation, expr2 = Matcher.match_with(ast,
				("binary", [
					tuple,
					(str, str),
					tuple,
				]))
			return self.compute_free_variables(expr1) | self.compute_free_variables(expr2)
		elif node_type in ["integer"]:
			return set()
		raise ValueError("Unhandled node type: %r" % (node_type,))

	def generate_bytecode_for_expr(self, expr, ctx):
		node_type, = Matcher.match_with(expr, (str, Matcher.DropAny()))

		if node_type == "literal":
			literal_class, literal_value = Matcher.match_with(expr, ("literal", [(str, str)]))
			ctx.append("# Lit: %r :: %r" % (literal_class, literal_value))
			if literal_class == "integer":
				ctx.append("MAKE_INT %i" % int(literal_value))
			elif literal_class == "string":
				ctx.append("MAKE_STRING %s" % literal_value.encode("hex"))
			else:
				assert False
		else:
			assert False

		ctx.append("# EXPR: %r" % (expr,))

	def generate_bytecode_for_seq(self, syntax_elem_seq, ctx):
		# Confirm that our input is a list of syntax elements.
		assert isinstance(syntax_elem_seq, list)
		assert all(isinstance(entry, SyntaxElement) for entry in syntax_elem_seq)
		ctx.append("# [%s]" % ", ".join(map(str, ctx.variable_table)))

		for syntax_elem in syntax_elem_seq:
			if syntax_elem.kind == "expr":
				pass # XXX
			elif syntax_elem.kind == "assignment":
				variable_name, expr = Matcher.match_with(syntax_elem.ast,
					("assignment", [
						("identifier", str),
						tuple,
					]))
				self.generate_bytecode_for_expr(expr, ctx)
				ctx.store(variable_name)
			elif syntax_elem.kind == "function_definition":
				pass # XXX
			else:
				raise ValueError("Unhandled case: %r" % (syntax_elem.kind,))

	def generate_overall_bytecode(self, syntax_elem_seq):
		# Confirm that our input is a list of syntax elements.
		assert isinstance(syntax_elem_seq, list)
		assert all(isinstance(entry, SyntaxElement) for entry in syntax_elem_seq)

		# First we compute the list of global variables.
		global_vars = self.compute_free_variables(syntax_elem_seq)
		# We now allocate them in some order for our global record.
		global_variable_table = [None] + sorted(global_vars)
		# Here the None corresponds to the argument our MAKE_FUNCTION will ignore.

		body = []
		output = ["MAKE_FUNCTION %s {" % len(global_variable_table), body, "}", "MAKE_NIL", "CALL"]
		ctx = ClCompiler.CompilationContext(body, global_variable_table, indent=2)
		self.generate_bytecode_for_seq(syntax_elem_seq, ctx)

		text = "\n".join(ClCompiler.flatten(output))
		return text

if __name__ == "__main__":
	_parser = ClParser()
	_ast = _parser.parse("""

# Huzzah for Cl!
x = 1
def foo asdf gkk
	y
	def bar
		z
	end
end

""")
	_compiler = ClCompiler()
	_bytecode_text = _compiler.generate_overall_bytecode(_ast)
	print _bytecode_text

	_ast = assemble.make_ast(_bytecode_text)
	__import__("pprint").pprint(_ast)
	_bytecode = assemble.assemble(_ast)
	print _bytecode.encode("hex")

	with open("bytecode.cl", "w") as f:
		f.write(_bytecode)

#	for obj in o:
#		obj.pprint()

