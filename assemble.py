#!/usr/bin/python

import struct, sys, re

# We will populate this list with opcodes parsed from cl.h.
opcodes = []

# Read cl.h, and filter down to the region between the tags "// BEGIN-PY-PARSING" and "// END-PY-PARSING".
with open("cl.h") as f:
	data = f.read()
data = data.split("// BEGIN-PY-PARSING")[1].split("// END-PY-PARSING")[0]

regex = re.compile('ClOpcodeDesc[(]{"(.+)",\s*([0-9]+),\s*(false|true)\s*}[)]')

for line in data.split("\n"):
	line = line.strip()
	match = regex.search(line)
	if match:
		opcode_name, opcode_args, opcode_data_field = match.groups()
		opcodes.append({
			"name": opcode_name,
			"args": int(opcode_args),
			"data_field": {"true": True, "false": False}[opcode_data_field],
		})

# Number the opcodes.
for i, op in enumerate(opcodes):
	op["index"] = i

mapping = {op["name"]: op for op in opcodes}

def make_ast(s):
	output = []
	s = s.replace(";", "\n")
	ast = []
	stack = [ast]

	for line in s.split("\n"):
		line = line.split("#")[0].strip()
		if not line:
			continue

		# We now pull out the first token to get the opcode name.
		opcode_name = line.split()[0]

		# If we hit a "}" then we pop out one level of function-definition.
		if opcode_name == "}":
			stack.pop()
			continue

		op = mapping[opcode_name]
		args = line.split(None, op["args"] + 1)

		# Strip off that first argument, that is just the opcode name.
		assert args[0] == opcode_name	
		args = args[1:]

		# MAKE_FUNCTION is handled totally separately.
		if opcode_name != "MAKE_FUNCTION":
			# Make sure that we have the right number of arguments, plus one if we have a data field.
			assert len(args) == op["args"] + op["data_field"]

			# Pull out the data field, if requested.
			data_field = None
			if op["data_field"]:
				args, data_field = args[:-1], args[-1]
				assert data_field.startswith('"') and data_field.endswith('"')
				data_field = data_field[1:-1]

			args = map(int, args)
			stack[-1].append({"name": opcode_name, "args": args, "data_field": data_field})
		else:
			# First off, in the case of a MAKE_FUNCTION, demand that the args end with "{", then strip it.
			assert len(args) == 1
			args = args[0].split()
			assert args.pop(-1) == "{"
			# We now read out the closure specifier.
			subscope_length = int(args[0])
			subscope_closure_descriptors = []
			for i in args[1:]:
				load_index, store_index = map(int, i.split("->"))
				subscope_closure_descriptors.append((load_index, store_index))
			new_ast_scope = []
			stack[-1].append({
				"name": "MAKE_FUNCTION",
				"args": [],
				"subscope_length": subscope_length,
				"subscope_closure_descriptors": subscope_closure_descriptors,
				"contents": new_ast_scope,
			})
			stack.append(new_ast_scope)

	assert len(stack) == 1, "Missing } to close some MAKE_FUNCTION!"

	return ast

def assemble(ast):
	output = []
	for op in ast:
		# Encode the 1-byte opcode.
		opcode_description = mapping[op["name"]]
		output.append(chr(opcode_description["index"]))

		# Encode the arguments.
		for arg in op["args"]:
			output.append(struct.pack("<q", arg))

		data_field = None

		# Encode the data-field, if present.
		if op["name"] == "MAKE_FUNCTION":
			# If we're a MAKE_FUNCTION then we format the data field specially.
			data_field = ""
			# First, include the subscope length.
			data_field += struct.pack("<I", op["subscope_length"])
			# Then include the number of closure descriptors.
			data_field += struct.pack("<I", len(op["subscope_closure_descriptors"]))
			# The include each pair.
			for load_index, store_index in op["subscope_closure_descriptors"]:
				data_field += struct.pack("<II", load_index, store_index)
			# Finally, include the recursively compiled sub-AST.
			data_field += assemble(op["contents"])
		else:
			data_field = op["data_field"]

		if data_field != None:
			output.append(struct.pack("<I", len(data_field)))
			output.append(data_field)

	return "".join(output)

	s = s.replace(";", "\n")
	for line in s.split("\n"):
		line = line.split("#")[0].strip()
		if not line:
			continue

		# We now pull out the first token to get the opcode name.
		opcode_name = line.split()[0]
		op = mapping[opcode_name]
		args = line.split(None, op["args"] + 1)

		# Strip off that first argument, that is just the opcode name.
		assert args[0] == opcode_name	
		args = args[1:]

		# Make sure that we have the right number of arguments, plus one if we have a data field.
		assert len(args) == op["args"] + op["data_field"]

		# Pull out the data field, if requested.
		if op["data_field"]:
			args, data_field = args[:-1], args[-1]
			assert data_field.startswith('"') and data_field.endswith('"')
			data_field = data_field[1:-1]

		# Do the actual encoding.
		output.append(chr(op["index"]))
		for arg in args:
			output.append(struct.pack("<q", int(arg)))
		if op["data_field"]:
			output.append(struct.pack("<I", len(data_field)))
			output.append(data_field)

	return "".join(output)

if __name__ == "__main__":
	with open("source.cla") as f:
		input_source = f.read()	
	ast = make_ast(input_source)
	__import__("pprint").pprint(ast)
	bytecode = assemble(ast)
	print bytecode.encode("hex")
	with open("bytecode.cl", "w") as f:
		f.write(bytecode)

