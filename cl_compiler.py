#!/usr/bin/python
# Python based Cl byte code compiler.
# This is a reference-only implementation in Python, before I launch into reimplementing this in C++.

import pprint, collections
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
		"class_block",
		"if_block",
		"else_block",
		"elif_block",
		"while_block",
		"for_block",
	])
	finalized = False

	def __init__(self, ast, line_number):
		self.ast = ast
		self.line_number = line_number
		self.kind, = Matcher.match_with(ast, (str, Matcher.DropAny()))
		self.block = [] if self.takes_block() else None
		self.else_trailer_block = None
		self.else_parent_block = None

	# This method is basically just for computing some values of function_definitions.
	# You can add other handlers here if you want, though.
	def finalize(self):
		if self.finalized:
			return
		self.finalized = True
		if self.kind == "function_definition":
			# First we compute the free variables in the body of the function...
			self.body_free = ClCompiler.compute_free_variables(self.block, locals_only=False)
			self.body_locals = ClCompiler.compute_free_variables(self.block, locals_only=True)
			# We now pull out the function name, and sequence of identifiers.
			self.function_name, self.args_seq = Matcher.match_with(self.ast,
				("function_definition", [
					("identifier", str),
					("ident_seq", list),
				]))
			self.args_variables = set()
			for entry in self.args_seq:
				arg, = Matcher.match_with(entry, ("identifier", str))
				self.args_variables.add(arg)

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
		class Accumulator:
			def __init__(self):
				self.tokens = []
				self.max_line_number = -1

		syntax_elements = [Accumulator()]
		for t in tokens:
			if t[0] == "newline":
				# If the current syntax element is empty, then don't bother splitting into another!
				if syntax_elements[-1].tokens == []:
					continue
				syntax_elements.append(Accumulator())
			else:
				# Here we strip the line_number entry from the individual
				# token, and update the accumulator's max_line)_number.
				pair = (t.cls, t.string)
				syntax_elements[-1].tokens.append(pair)
				syntax_elements[-1].max_line_number = max(syntax_elements[-1].max_line_number, t.line_number)
		# Strip the potential trailing empty syntax element.
		if syntax_elements[-1].tokens == []:
			syntax_elements.pop(-1)

		# Build a derivation for each syntax element.
		derived_syntax_elements = []
		for syntax_element in syntax_elements:
			derivations = list(self.bnf_parser("syntax_element", syntax_element.tokens))
			if len(derivations) == 0:
				print "Syntax error on:", self.detokenize(syntax_element.tokens)
				exit(1)
			elif len(derivations) > 1:
				print "BUG BUG BUG: Ambiguous syntax on:", self.detokenize(syntax_element.tokens)
				for i, derivation in enumerate(derivations):
					print
					print "===== Derivation %i:" % (i + 1)
					__import__("pprint").pprint(derivation)
				exit(2)
			# Extract the unique derivation of this syntax element.
			derivation = derivations[0]
			# This becomes a derived syntax element.
			derived_syntax_elements.append((derivation, syntax_element.max_line_number))

		# We now assemble the syntax elements into an AST, and in particular, enforce various block nesting rules at this point.
		output = []
		fake = collections.namedtuple("fake", ["block"])
		stack = [fake(output)]
		all_syntax_elements = []
		for derivation, max_line_number in derived_syntax_elements:
			# This derivation will always be of the form ("syntax_element", [(kind of syntax element, [...])])
			# Let's pull out this kind, while simultaneously sanity checking the structure.
			main_element_kind, main_element_contents = Matcher.match_with(derivation,
				("syntax_element", [(str, list)])
			)
			# Build a wrapped SyntaxElement object...
			se = SyntaxElement(ast=(main_element_kind, main_element_contents), line_number=max_line_number)
			all_syntax_elements.append(se)
			# ... and insert it at the appropriate place in the tree.
			if se.kind != "end":
				stack[-1].block.append(se)

			# However, if the syntax element is a block ender, then we simplify the tree.
			if se.kind in ["end", "else_block", "elif_block"]:
				if len(stack) > 1:
					closing_syntax_element = stack.pop()
					if se.kind in ["else_block", "elif_block"]:
						if closing_syntax_element.kind not in ["if_block", "elif_block", "for_block", "while_block"]:
							print "Misplaced else-style block, after: %s" % closing_syntax_element.kind
						# Link the else-style block to its parent.
						closing_syntax_element.else_trailer_block = se
						se.else_parent_block = closing_syntax_element
				else:
					print "Misplaced %s, with no block to end." % se.kind
					exit(1)

			# If the syntax element wants a block, then we complexify our tree.
			if se.takes_block():
				stack.append(se)

		# Finalize all the syntax elements.
		for se in all_syntax_elements:
			se.finalize()

		# Finally, we demand that the stack be simply [output] at the end, otherwise an end was mismatched.
		if len(stack) > 1:
			print "Unended block(s)."
			exit(1)
		assert len(stack) == 1 and stack[0].block == output

		return output

class ClCompiler:
	def __init__(self):
		self.label_counter = 0

	def new_label(self):
		self.label_counter += 1
		return "l%i" % self.label_counter

	class CompilationContext:
		def __init__(self, target, variable_table, indent=0):
			self.target, self.variable_table = target, variable_table
			self.indent = indent
			self.break_labels = []
			self.continue_labels = []

		def load(self, var):
			if var in self.variable_table:
				self.append("LOAD %i    # %s" % (self.variable_table.index(var), var))
			else:
				self.append("GET_GLOBAL")
				self.append("DOT_LOAD \"%s\"" % (var,))

		def store(self, var):
			if var in self.variable_table:
				self.append("STORE %i   # %s" % (self.variable_table.index(var), var))
			else:
				self.append("GET_GLOBAL")
				self.append("DOT_STORE \"%s\"" % (var,))

		def append(self, x):
			for part in x.split("\n"):
				self.target.append(" " * self.indent + part)

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

	@staticmethod
	def decode_string_constant(s):
		assert s.startswith('"') and s.endswith('"')
		# TODO: Handle escapes here.
		return s[1:-1]

	@staticmethod
	def compute_free_variables(ast, locals_only):
		# For lists, union over entries.
		if isinstance(ast, list):
			free = set()
			for entry in ast:
				free |= ClCompiler.compute_free_variables(entry, locals_only)
			return free

		if isinstance(ast, SyntaxElement):
			node_type = ast.ast[0]
			if node_type in ["expr", "return", "debug_print"]:
				return ClCompiler.compute_free_variables(ast.ast[1], locals_only)
			elif node_type == "expr": # XXX This code path disabled for now.
				subexpression, = Matcher.match_with(ast.ast, ("expr", [tuple]))
				return ClCompiler.compute_free_variables(subexpression, locals_only)
			elif node_type == "assignment":
				assign_spec, expr = Matcher.match_with(ast.ast,
					("assignment", [
						tuple,
						tuple,
					]))
				def get_variables_from_assign_spec(assign_spec):
					"""get_variables_from_assign_spec(assign_spec) -> set of variables
					The same deal as compute_free_variables, but it knows about unpack_specs.
					This is a separate function because variables are different in the context
					of an assignment, and rather than propagating this state as an argument
					to compute_free_variables, we simply define a different function here.
					"""
					kind = assign_spec[0]
					if kind == "variable":
						variable_name, = Matcher.match_with(assign_spec, ("variable", [("identifier", str)]))
						return set([variable_name])
					elif kind in ["dot_accessor", "indexing"]:
						return ClCompiler.compute_free_variables(assign_spec, locals_only)
					elif kind == "unpack_spec":
						assignees = set()
						for subassign in assign_spec[1]:
							assignees |= get_variables_from_assign_spec(subassign)
						return assignees
					else:
						raise ValueError("Unhandled assign_spec kind: %r" % (kind,))
				return get_variables_from_assign_spec(assign_spec) | \
				       ClCompiler.compute_free_variables(expr, locals_only)
			elif node_type == "function_definition":
				ast.finalize()
				if not locals_only:
					return set([ast.function_name])
				else:
					# If we're computing the closure locals, then we add in those from inside recursively.
					return set([ast.function_name]) | ClCompiler.compute_free_variables(ast.block, locals_only)
			elif node_type in ("while_block", "if_block", "else_block", "elif_block", "for_block", "class_block"):
				return ClCompiler.compute_free_variables(ast.ast[1], locals_only) | ClCompiler.compute_free_variables(ast.block, locals_only)
			raise ValueError("Unhandled syntax element type: %r" % (ast.ast,))

		# At this point we are guaranteed to be a tuple from the raw AST given by our parser.
		node_type, = Matcher.match_with(ast, (str, Matcher.DropAny()))

		if node_type in ["literal", "variable", "unpack_spec", "list_comp"]:
			return ClCompiler.compute_free_variables(ast[1], locals_only)
		elif node_type == "identifier":
			# If we're computing local variables then a simple reference isn't enough -- you have to assign.
			if locals_only:
				return set([])
			else:
				return set([ast[1]])
		elif node_type == "lambda":
			variable_name, expr = Matcher.match_with(ast,
				("lambda", [
					("identifier", str),
					tuple,
				]))
			# We return the free variables in the function body, *minus* the bound variable.
			return ClCompiler.compute_free_variables(expr, locals_only) - set([variable_name])
		elif node_type in ["function_call", "list_literal"]:
			 # The free variables are simply the union of those in each sub-tuple.
			free = set()
			for obj in ast[1]:
				if isinstance(obj, tuple):
					free |= ClCompiler.compute_free_variables(obj, locals_only)
			return free
		elif node_type == "binary":
			expr1, operation_class, operation, expr2 = Matcher.match_with(ast,
				("binary", [
					tuple,
					(str, str),
					tuple,
				]))
			return ClCompiler.compute_free_variables(expr1, locals_only) | ClCompiler.compute_free_variables(expr2, locals_only)
		elif node_type == "dot_accessor":
			expr, dot_access_name = Matcher.match_with(ast,
				("dot_accessor", [
					tuple,
					("identifier", str),
				]))
			return ClCompiler.compute_free_variables(expr, locals_only)
		elif node_type == "indexing":
			expr1, expr2 = Matcher.match_with(ast,
				("indexing", [
					tuple,
					tuple,
				]))
			return ClCompiler.compute_free_variables(expr1, locals_only) | \
			       ClCompiler.compute_free_variables(expr2, locals_only)
		elif node_type == "list_comp_group":
			# XXX: Currently this produces a local because it's used only in for loops.
			# Later, when it's used in actually list comprehensions, it will have to change.
			assign_spec, expr = Matcher.match_with(ast,
				("list_comp_group", [
					tuple,
					tuple,
				]))
			return ClCompiler.compute_free_variables(assign_spec, locals_only) | \
			       ClCompiler.compute_free_variables(expr, locals_only)
		elif node_type in ["integer", "string", "this"]:
			return set()
		raise ValueError("Unhandled node type: %r" % (node_type,))

	def generate_consumer_for_unpack_spec(self, unpack_spec_contents, ctx):
		ctx.append("DOT_LOAD \"iter\"")
		# Make a label for unpack too few.
		too_few_label = self.new_label()
		# Begin pulling out values, and assigning.
		for assignment_target in unpack_spec_contents:
			ctx.append("ITERATE %s" % too_few_label)
			self.generate_consumer_for_assign_spec(assignment_target, ctx)
		# Finally, iterate one last time, to rule out too many.
		good_label = self.new_label()
		ctx.append("ITERATE %s" % good_label)
		ctx.append("TRACEBACK \"Too many values to unpack.\"")
		ctx.append("%s:" % too_few_label)
		ctx.append("TRACEBACK \"Too few values to unpack.\"")
		ctx.append("%s:" % good_label)
		ctx.append("POP")

	def generate_consumer_for_assign_spec(self, assign_spec, ctx):
		target_type, target_contents = Matcher.match_with(assign_spec, (str, list))
		if target_type == "unpack_spec":
			self.generate_consumer_for_unpack_spec(target_contents, ctx)
		elif target_type == "variable":
			variable_name, = Matcher.match_with(target_contents, [("identifier", str)])
			ctx.store(variable_name)
		elif target_type == "dot_accessor":
			# Pull out the expression and variable name in the node matching "expr . identifier":
			expr, index_variable_name = Matcher.match_with(target_contents,
				[tuple, ("identifier", str)])
			self.generate_bytecode_for_expr(expr, ctx)
			ctx.append("DOT_STORE \"%s\"" % index_variable_name)
		elif target_type == "indexing":
			expr1, expr2 = Matcher.match_with(target_contents, [tuple, tuple])
			self.generate_bytecode_for_expr(expr1, ctx)
			self.generate_bytecode_for_expr(expr2, ctx)
			ctx.append("STORE_INDEX")
		else:
			raise Exception("Unhandled assignment spec type: %r." % (spec_kind,))

	def generate_bytecode_for_expr(self, expr, ctx):
		node_type, = Matcher.match_with(expr, (str, Matcher.DropAny()))

		if node_type == "literal":
			literal_class, literal_value = Matcher.match_with(expr, ("literal", [(str, str)]))
#			ctx.append("# Lit: %r :: %r" % (literal_class, literal_value))
			if literal_class == "integer":
				ctx.append("MAKE_INT %i" % int(literal_value))
			elif literal_class == "string":
				string_spec = ClCompiler.decode_string_constant(literal_value).encode("hex")
				ctx.append("MAKE_STRING %s" % string_spec)
			elif literal_class == "identifier":
				ctx.load(literal_value)
			else:
				raise ValueError("Unhandled expr literal type: %r" % (literal_class,))
		elif node_type == "list_literal":
			ctx.append("MAKE_LIST")
			for entry_expr in expr[1]:
				self.generate_bytecode_for_expr(entry_expr, ctx)
				ctx.append("LIST_APPEND")
		elif node_type == "list_comp":
			term_expr, assign_spec, iter_expr = Matcher.match_with(expr,
				("list_comp", [
					tuple,
					("list_comp_group", [
						tuple,
						tuple,
					]),
				]))
			ctx.append("MAKE_LIST")
			self.generate_bytecode_for_expr(iter_expr, ctx)
			top_label = self.new_label()
			done_iterating_label = self.new_label()
			ctx.append("DOT_LOAD \"iter\"")
			ctx.append("%s:" % top_label)
			ctx.append("ITERATE %s" % done_iterating_label)
			# Then assign the yielded value into the iteration variable.
			self.generate_consumer_for_assign_spec(assign_spec, ctx)
			ctx.append("SWAP")
			self.generate_bytecode_for_expr(term_expr, ctx)
			ctx.append("LIST_APPEND")
			ctx.append("SWAP")
			ctx.append("JUMP %s" % top_label)
			ctx.append("%s:" % done_iterating_label)
			ctx.append("POP")
		elif node_type == "function_call":
			expr1, expr2 = Matcher.match_with(expr, ("function_call", [tuple, tuple]))
			self.generate_bytecode_for_expr(expr1, ctx)
			self.generate_bytecode_for_expr(expr2, ctx)
			ctx.append("CALL")
		elif node_type == "dot_accessor":
			sub_expr, dot_access_name = Matcher.match_with(expr,
				("dot_accessor", [
					tuple,
					("identifier", str),
				]))
			self.generate_bytecode_for_expr(sub_expr, ctx)
			ctx.append("DOT_LOAD \"%s\"" % dot_access_name)
		elif node_type == "indexing":
			expr1, expr2 = Matcher.match_with(expr, ("indexing", [tuple, tuple]))
			self.generate_bytecode_for_expr(expr1, ctx)
			self.generate_bytecode_for_expr(expr2, ctx)
			ctx.append("BINARY_INDEX")
		elif node_type == "binary":
			expr1, operation_class, operation, expr2 = Matcher.match_with(expr,
				("binary", [
					tuple,
					(str, str),
					tuple,
				]))
			self.generate_bytecode_for_expr(expr1, ctx)
			self.generate_bytecode_for_expr(expr2, ctx)
			# Generate based on the operation.
			mapping = {
				"+": "BINARY_PLUS",
				"-": "BINARY_MINUS",
				"*": "BINARY_TIMES",
				"/": "BINARY_DIVIDE",
				"%": "BINARY_MODULO",
				"in": "BINARY_IN",
				"==": "BINARY_COMPARE 0",
				"!=": "BINARY_COMPARE 1",
				"<": "BINARY_COMPARE 2",
				">": "BINARY_COMPARE 3",
				"<=": "BINARY_COMPARE 4",
				">=": "BINARY_COMPARE 5",
			}
			if operation not in mapping:
				raise ValueError("Unhandled binary operation type: %r" % (operation,))
			ctx.append(mapping[operation])
		elif node_type == "unary":
			operation_class, operation, expr = Matcher.match_with(expr,
				("unary", [
					(str, str),
					tuple,
				]))
			self.generate_bytecode_for_expr(expr, ctx)
			mapping = {
				"-": "UNARY_MINUS",
			}
			if operation not in mapping:
				raise ValueError("Unhandled unary operation type: %r" % (operation,))
			ctx.append(mapping[operation])
		elif node_type == "this":
			ctx.append("GET_THIS")
		elif node_type == "lambda":
			variable_name, expr = Matcher.match_with(expr,
				("lambda", [
					("identifier", str),
					tuple,
				]))
			raise NotImplementedError
		else:
			raise ValueError("Unhandled expr node_type type: %r" % (node_type,))

#		ctx.append("# EXPR: %r" % (expr,))

	def generate_bytecode_for_seq(self, syntax_elem_seq, ctx, class_mode=False):
		# Confirm that our input is a list of syntax elements.
		assert isinstance(syntax_elem_seq, list)
		assert all(isinstance(entry, SyntaxElement) for entry in syntax_elem_seq)
		ctx.append("# [%s]" % ", ".join(map(str, ctx.variable_table)))

		for syntax_elem in syntax_elem_seq:
			ctx.append("  LINE_NUMBER %i" % syntax_elem.line_number)
			if syntax_elem.kind == "expr":
				expr, = Matcher.match_with(syntax_elem.ast, ("expr", [tuple]))
				# Generate the value then immediately drop it.
				self.generate_bytecode_for_expr(expr, ctx)
				ctx.append("POP")
			elif syntax_elem.kind == "return":
				expr_list, = Matcher.match_with(syntax_elem.ast, ("return", list))
				# If expr_list is empty, then we're an argumentless return, and return nil.
				if len(expr_list) == 0:
					ctx.append("MAKE_NIL")
				else:
					# Otherwise, generate the return value, and return it.
					self.generate_bytecode_for_expr(expr_list[0], ctx)
				ctx.append("RETURN")
			elif syntax_elem.kind == "debug_print":
				expr, = Matcher.match_with(syntax_elem.ast, ("debug_print", [tuple]))
				self.generate_bytecode_for_expr(expr, ctx)
				ctx.append("PRINT")
			elif syntax_elem.kind == "assignment":
				assign_spec, expr = Matcher.match_with(syntax_elem.ast,
					("assignment", [
						tuple,
						tuple,
					]))
				self.generate_bytecode_for_expr(expr, ctx)
				# We now compute the assignment target parameters.
				self.generate_consumer_for_assign_spec(assign_spec, ctx)
#			elif syntax_elem.kind == "unpack_assignment":
#				unpack_spec, expr = Matcher.match_with(syntax_elem.ast,
#					("unpack_assignment", [
#						("unpack_spec", list),
#						tuple,
#					]))
#				# First, we evaluate expr, and then we iterate over it.
#				self.generate_bytecode_for_expr(expr, ctx)
#				self.generate_consumer_for_unpack_spec(unpack_spec, ctx)
			elif syntax_elem.kind in ["while_block", "if_block"]:
				expr, = Matcher.match_with(syntax_elem.ast, (syntax_elem.kind, [tuple]))
				# Make a label to jump to for testing, and to jump to when done.
				top_label = self.new_label()
				syntax_elem.bottom_label = self.new_label()
				ctx.append("%s:" % top_label)
				self.generate_bytecode_for_expr(expr, ctx)
				ctx.append("JUMP_IF_FALSEY %s" % syntax_elem.bottom_label)
				# Make new entries for continue and break.
				if syntax_elem.kind == "while_block":
					ctx.continue_labels.append(top_label)
					ctx.break_labels.append(syntax_elem.bottom_label)
				self.generate_bytecode_for_seq(syntax_elem.block, ctx)
				if syntax_elem.kind == "while_block":
					ctx.continue_labels.pop()
					ctx.break_labels.pop()
					ctx.append("JUMP %s" % top_label)
				# If we have an else-trailer block, then we let them define where we jump.
				if syntax_elem.else_trailer_block == None:
					ctx.append("%s:" % syntax_elem.bottom_label)
			elif syntax_elem.kind in ["else_block", "elif_block"]:
				syntax_elem.bottom_label = self.new_label()
				ctx.append("JUMP %s" % syntax_elem.bottom_label)
				if syntax_elem.kind == "elif_block":
					expr, = Matcher.match_with(syntax_elem.ast, ("elif_block", [tuple]))
					self.generate_bytecode_for_expr(expr, ctx)
					ctx.append("JUMP_IF_FALSEY %s" % syntax_elem.bottom_label)
				# Produce the long-prophesied bottom label for our parent block.
				ctx.append("%s: " % syntax_elem.else_parent_block.bottom_label)
				self.generate_bytecode_for_seq(syntax_elem.block, ctx)
				if syntax_elem.else_trailer_block == None:
					ctx.append("%s: " % syntax_elem.bottom_label)
			elif syntax_elem.kind == "for_block":
				# Make a label for breaking out of the loop.
				assign_spec, expr = Matcher.match_with(syntax_elem.ast,
					("for_block", [
						("list_comp_group", [
							tuple,
							tuple,
						])
					]))
				top_label = self.new_label()
				done_iterating_label = self.new_label()
				# Generate the iterator object, then iterate over it.
				ctx.continue_labels.append(top_label)
				ctx.break_labels.append(done_iterating_label)
				self.generate_bytecode_for_expr(expr, ctx)
				ctx.continue_labels.pop()
				ctx.break_labels.pop()
				ctx.append("DOT_LOAD \"iter\"")
				ctx.append("%s:" % top_label)
				ctx.append("ITERATE %s" % done_iterating_label)
				# Then assign the yielded value into the iteration variable.
				self.generate_consumer_for_assign_spec(assign_spec, ctx)
				# Perform the block.
				self.generate_bytecode_for_seq(syntax_elem.block, ctx)
				ctx.append("JUMP %s" % top_label)
				ctx.append("%s:" % done_iterating_label)
				# Drop the iterator object that just returned a ClStopIteration.
				ctx.append("POP")
			elif syntax_elem.kind in ["break", "continue"]:
				number_list, = Matcher.match_with(syntax_elem.ast, (syntax_elem.kind, list))
				goto_index = 1
				if len(number_list) != 0:
					goto_index, = Matcher.match_with(number_list, [("integer", str)])
					goto_index = int(goto_index)
				label_list = ctx.break_labels if syntax_elem.kind == "break" else ctx.continue_labels
				if goto_index <= 0 or goto_index > len(label_list):
					print "Break/continue index out of range."
					exit(1)
				ctx.append("JUMP %s" % label_list[-goto_index])
			elif syntax_elem.kind == "function_definition":
				# Generate an appropriate closure.
				# The full inner variable table is just all the free variables used inside.
				assert len(syntax_elem.args_seq) == 1, "For now only functions of one variable are supported."
				argument_name, = Matcher.match_with(syntax_elem.args_seq[0], ("identifier", str))
				inner_variable_table = list(syntax_elem.body_locals - set([argument_name]))
				# Map the function argument to be the first entry in the variable table.
				inner_variable_table = [argument_name] + inner_variable_table

				# Compute the list of variables we're closing over.
				print "\n\nDETERMINING THE CLOSURE VARIABLES."
				print "I have this variable table:", ctx.variable_table
				print "I have these body_locals:", syntax_elem.body_locals
				print "I have these body_free:", syntax_elem.body_free
				print "Args variables:", syntax_elem.args_variables

				# We close precisely when the variable in question is referenced, but not
				# assigned in the function, and we have the variable in our variable table.

				closure_vars = syntax_elem.body_free - syntax_elem.body_locals - syntax_elem.args_variables
				print "Closure candidates:", closure_vars
				closure_vars &= set(ctx.variable_table)
				print "Final closure:", closure_vars
				print

				transfer_records = [
					(ctx.variable_table.index(var), inner_variable_table.index(var))
					for var in closure_vars if var in ctx.variable_table
				]
				transfer_records_string = "".join(" %i->%i" % pair for pair in transfer_records)
				body = []
				definition = ["MAKE_FUNCTION %s %s%s {" % (syntax_elem.function_name, len(inner_variable_table), transfer_records_string), body, "}"]
				inner_ctx = ClCompiler.CompilationContext(body, inner_variable_table, indent=2)
				self.generate_bytecode_for_seq(syntax_elem.block, inner_ctx)
				# Finally, we flatten our definition.
				def_string = "\n".join(self.flatten(definition))
				ctx.append(def_string)
				# If we're in class mode, then we mark the function as a method.
				if class_mode:
					ctx.load("methodify")
					ctx.append("SWAP")
					ctx.append("CALL")
				# We now have the function on the stack, so assign it into its name.
				ctx.store(syntax_elem.function_name)
			elif syntax_elem.kind == "class_block":
				class_name, = Matcher.match_with(syntax_elem.ast, ("class_block", [("identifier", str)]))
				# Change the global scope appropriately.
				ctx.append("GET_GLOBAL")
				ctx.append("DUP")
				ctx.append("MAKE_INSTANCE_P")
				ctx.append("DUP")
				# Stack: [old global, new global, new global]
				# Store the class name before we modify the global context!
				ctx.store(class_name)
				ctx.append("SET_GLOBAL")
				# Now begin our definitions.
				self.generate_bytecode_for_seq(syntax_elem.block, ctx, class_mode=True)
				# Finally, reset the global scope.
				ctx.append("SET_GLOBAL")
			elif syntax_elem.kind == "literal_cl_assembly":
				asm, = Matcher.match_with(syntax_elem.ast, ("literal_cl_assembly", [("literal_cl_assembly", str)]))
				assert asm.startswith(">>>")
				asm = asm[3:]
				ctx.append(asm)
			else:
				raise ValueError("Unhandled case: %r" % (syntax_elem.kind,))

	def generate_overall_bytecode(self, syntax_elem_seq):
		# Confirm that our input is a list of syntax elements.
		assert isinstance(syntax_elem_seq, list)
		assert all(isinstance(entry, SyntaxElement) for entry in syntax_elem_seq)

		# First we compute the list of global variables.
		global_vars = set() # = self.compute_local_variables(syntax_elem_seq, locals_only=True)
		# We now allocate them in some order for our global record.
		global_variable_table = [None] + sorted(global_vars)
		# Here the None corresponds to the argument our MAKE_FUNCTION will ignore.

		body = []
		output = ["MAKE_FUNCTION <root> %s {" % len(global_variable_table), body, "}", "MAKE_NIL", "CALL"]
		ctx = ClCompiler.CompilationContext(body, global_variable_table, indent=2)
		self.generate_bytecode_for_seq(syntax_elem_seq, ctx)

		text = "\n".join(ClCompiler.flatten(output))
		return text

def source_to_bytecode(source, source_file_path="<sourceless>"):
	parser = ClParser()
	compiler = ClCompiler()
	ast = parser.parse(source)
	bytecode_text = compiler.generate_overall_bytecode(ast)
#	print "Bytecode text:", bytecode_text
	assembly_unit = assemble.make_assembly_unit(bytecode_text)
	bytecode = assemble.assemble(assembly_unit, source_file_path)
	return bytecode

if __name__ == "__main__":
	source = """

def func x
	class Foo
	end
	b = Foo(nil)
	b.x = [1,2,3,4,5]
	a, (b.x)[3], c = ["a", "b", "c"]
	print b.x
end

func(nil)

END_CL_INPUT

#a = [ [ i * j | j <- upto(i)] | i <- upto(5) ]

def func x
#	f = \k -> k+1

	a = [ i | i <- upto(10) ]
	b = 2
	a, b = [b, a]

	class Foo
		value = 0
		def another x
			@.v = @
			return @ + @
		end
	end
end

func(nil)
print a
for i <- a
	print i
end

END_CL_INPUT

class Foo
	value = 0

	def add x
		@.value = @.value + x
		return @.value
	end
end

f = Foo(nil)

print f.add(10)
print f.add(10)

for i <- upto(10)
	print i
end

l = []

while len(l) < 10
	if len(l) < 5
		l.append(1)
	else
		break 1
		l.append(2)
	end
end

print l


# Huzzah for Cl!
def build_adder x
	def the_adder y
		return x + y
	end
	return the_adder
end

five_adder = build_adder([5])
print five_adder([7])
print 5.to_string(nil)

l = []
l.append("foo"); l.append(2)
l.append(3)
for i <- l
	print i
end

print len(l)

def factorial y
	accum = 1
	while y
		accum = accum * y
		y = y - 1
	end
	return accum
end
print factorial(5)

"""
	_bytecode = source_to_bytecode(source)

	with open("bytecode.clo", "w") as f:
		f.write(_bytecode)

#	for obj in o:
#		obj.pprint()

