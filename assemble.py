#!/usr/bin/python

import struct, sys, re

# We will populate this list with opcodes parsed from cl.h.
opcodes = []

# Read cl.h, and filter down to the region between the tags "// BEGIN-PY-PARSING" and "// END-PY-PARSING".
with open("cl.h") as f:
	data = f.read()
data = data.split("// BEGIN-PY-PARSING")[1].split("// END-PY-PARSING")[0]

regex = re.compile('ClOpcodeDesc[(]{"(.+)",\s*([0-9]+),\s*([-0-9]+),\s*(false|true)\s*}[)]')

for line in data.split("\n"):
	line = line.strip()
	match = regex.search(line)
	if match:
		opcode_name, opcode_args, opcode_stack_delta, opcode_data_field = match.groups()
		opcodes.append({
			"name": opcode_name,
			"args": int(opcode_args),
			"stack_delta": int(opcode_stack_delta),
			"data_field": {"true": True, "false": False}[opcode_data_field],
		})

# Number the opcodes.
for i, op in enumerate(opcodes):
	op["index"] = i

mapping = {op["name"]: op for op in opcodes}

class AssemblyUnit:
	"""AssemblyUnit

	Corresponds to a scope in which instruction counters are contiuous.
	Thus, labels and jumps only make sense at the level of an AU.
	Each function has its own AU, as well as the global level.
	"""
	def __init__(self):
		self.labels = {}
		self.ast = []
		self.instruction_counter = 0

def make_assembly_unit(s):
	s = s.replace(";", "\n")
	assembly_unit = AssemblyUnit()
	stack = [assembly_unit]

	for line in s.split("\n"):
		line = line.split("#")[0].strip()
		if not line:
			continue

		# If the line ends with a colon, then it's a label.
		if line.endswith(":"):
			label_name = line[:-1]
			stack[-1].labels[label_name] = stack[-1].instruction_counter
			continue

		# We now pull out the first token to get the opcode name.
		opcode_name = line.split()[0]

		# Only increment the instruction counter if we're not a LINE_NUMBER pseudo-instruction.
		if opcode_name != "LINE_NUMBER":
			stack[-1].instruction_counter += 1

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
				# If in quotes, the field is a literal, otherwise it's a hex encoding.
				if data_field.startswith('"') and data_field.endswith('"'):
					data_field = data_field[1:-1]
				else:
					data_field = data_field.decode("hex")

			stack[-1].ast.append({"name": opcode_name, "args": args, "data_field": data_field})
		else:
			# First off, in the case of a MAKE_FUNCTION, demand that the args end with "{", then strip it.
			assert len(args) == 1
			args = args[0].split()
			assert args.pop(-1) == "{"
			function_name = args[0]
			function_argument_count = int(args[1])
			# We now read out the closure specifier.
			subscope_length = int(args[2])
			subscope_closure_descriptors = []
			for i in args[3:]:
				load_index, store_index = map(int, i.split("->"))
				subscope_closure_descriptors.append((load_index, store_index))
			new_assembly_unit = AssemblyUnit()
			stack[-1].ast.append({
				"name": "MAKE_FUNCTION",
				"args": [],
				"function_name": function_name,
				"function_argument_count": function_argument_count,
				"subscope_length": subscope_length,
				"subscope_closure_descriptors": subscope_closure_descriptors,
				"contents": new_assembly_unit,
			})
			stack.append(new_assembly_unit)

	assert len(stack) == 1, "Missing } to close some MAKE_FUNCTION!"

	return assembly_unit

def assemble(assembly_unit, source_file_path):
	output = []
	for op in assembly_unit.ast:
		# Encode the 1-byte opcode.
		opcode_description = mapping[op["name"]]
		output.append(chr(opcode_description["index"]))

		# If we're a jump, do special handling to compute our argument.
		is_jump = op["name"] in ["JUMP", "JUMP_IF_TRUTHY", "JUMP_IF_FALSEY", "ITERATE"]
		if is_jump:
			op["args"] = map(assembly_unit.labels.__getitem__, op["args"])

		# Encode the arguments.
		for arg in op["args"]:
			output.append(struct.pack("<q", int(arg)))

		data_field = None

		# Encode the data-field, if present.
		if op["name"] == "MAKE_FUNCTION":
			# If we're a MAKE_FUNCTION then we format the data field specially.
			data_field = ""
			# First include the function name.
			data_field += struct.pack("<I", len(op["function_name"]))
			data_field += op["function_name"]
			# Next include the file path.
			source_file_path = source_file_path
			data_field += struct.pack("<I", len(source_file_path))
			data_field += source_file_path
			# Include the argument count.
			data_field += struct.pack("<I", op["function_argument_count"])
			# Next, include the subscope length.
			data_field += struct.pack("<I", op["subscope_length"])
			# Then include the number of closure descriptors.
			data_field += struct.pack("<I", len(op["subscope_closure_descriptors"]))
			# The include each pair.
			for load_index, store_index in op["subscope_closure_descriptors"]:
				data_field += struct.pack("<II", load_index, store_index)
			# Finally, include the recursively compiled sub-AST.
			data_field += assemble(op["contents"], source_file_path)
		else:
			data_field = op["data_field"]

		if data_field != None:
			output.append(struct.pack("<I", len(data_field)))
			output.append(data_field)

	return "".join(output)

if __name__ == "__main__":
	with open("source.cla") as f:
		_input_source = f.read()	
	_assembly_unit = make_assembly_unit(_input_source)
	__import__("pprint").pprint(_assembly_unit)
	_bytecode = assemble(_assembly_unit, "<sourceless>")
	print _bytecode.encode("hex")
	with open("bytecode.clo", "w") as f:
		f.write(_bytecode)

