#!/usr/bin/python
# Python based Cl byte code compiler.
# This is a reference-only implementation in Python, before I launch into reimplementing this in C++.

import pprint
import parse
import assemble

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
			assert derivation[0] == "syntax_element"
			assert len(derivation) == 2
			assert len(derivation[1]) == 1
			main_element = derivation[1][0]
			assert len(main_element) == 2
			main_element_kind = main_element[0]
			# Build a wrapped SyntaxElement object...
			se = SyntaxElement(main_element_kind, main_element)
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
	def __init__(self):
		pass

	def compute_free_variables(self, ast):
		free = set()
		# For lists, union over entries.
		if isinstance(ast, list):
			for entry in ast:
				free |= self.compute_free_variables(entry)
			return free
		# Otherwise, handle by cases.
#		if ast.kind in ["expr"]:
#			for entry in ast.ast

		free = set()
		for obj in ast:
			obj.pprint()

	def generate_bytecode(self, ast):
		for obj in ast: obj.pprint()
#		global_vars = self.compute_free_variables(ast)

if __name__ == "__main__":
	parser = ClParser()
	ast = parser.parse("""

# Huzzah for Cl!
x
def foo()
	y
	def bar()
		z
	end
end

""")
	compiler = ClCompiler()
	bytecode = compiler.generate_bytecode(ast)
	print bytecode

#	for obj in o:
#		obj.pprint()

