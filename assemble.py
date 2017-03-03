#!/usr/bin/python

import struct, sys, re

# We will populate this list with opcodes parsed from cl.h.
opcodes = []

# Read cl.h, and filter down to the region between the tags "// BEGIN-PY-PARSING" and "// END-PY-PARSING".
with open("cl.h") as f:
	data = f.read()
data = data.split("// BEGIN-PY-PARSING")[1].split("// END-PY-PARSING")[0]

regex = re.compile('ClOpcodeDesc[(]{"(.+)", ([0-9]+)}[)]')

for line in data.split("\n"):
	line = line.strip()
	match = regex.search(line)
	if match:
		opcode_name, opcode_args = match.groups()
		opcodes.append({
			"name": opcode_name,
			"args": int(opcode_args)
		})

# Number the opcodes.
for i, op in enumerate(opcodes):
	op["index"] = i

mapping = {op["name"]: op for op in opcodes}

def assemble(s):
	output = []
	s = s.replace(";", "\n")
	for line in s.split("\n"):
		line = line.split("#")[0].strip()
		if not line:
			continue

		args = line.split()
		opcode_name = args[0]
		op = mapping[opcode_name]
		assert len(args) == op["args"] + 1

		# Do the actual encoding.
		output.append(chr(op["index"]))
		for arg in args[1:]:
			output.append(struct.pack("<q", int(arg)))
	return "".join(output)

if __name__ == "__main__":
	input_source = sys.stdin.read()
	output = assemble(input_source)
	print >>sys.stderr, output.encode("hex")
	sys.stdout.write(output)
	sys.stdout.flush()

