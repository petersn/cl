#!/usr/bin/python

import ctypes
import cl_compiler
import assemble

cl_shared_object = ctypes.CDLL("./cl.so")

_cl_execute_string = cl_shared_object.cl_execute_string
_cl_execute_string.argtypes = [ctypes.c_char_p, ctypes.c_int]
_cl_execute_string.restype = None

cl_make_context = cl_shared_object.cl_make_context
cl_make_context.argtypes = []
cl_make_context.restype = ctypes.c_void_p

cl_execute_string_in_context = cl_shared_object.cl_execute_string_in_context
cl_execute_string_in_context.argtypes = [ctypes.c_void_p, ctypes.c_char_p, ctypes.c_int]
cl_execute_string_in_context.restype = None

cl_free_context = cl_shared_object.cl_free_context
cl_free_context.argtypes = [ctypes.c_void_p]
cl_free_context.restype = None

def cl_execute_string(s):
	_cl_execute_string(s, len(s))

class ClContext:
	def __init__(self):
		self.ctx_ptr = cl_make_context()
		# Save this pointer for del time.
		# This is necessary, as globals might disappear at del time.
		self.cl_free_context = cl_free_context

	def __del__(self):
		self.free()

	def execute(self, s):
		cl_execute_string_in_context(self.ctx_ptr, s, len(s))

	def free(self):
		if self.ctx_ptr:
			self.cl_free_context(self.ctx_ptr)
			self.ctx_ptr = None

if __name__ == "__main__":
	import sys
	import readline

	if len(sys.argv) not in (1, 2):
		print "Usage: cl.py [input.cl]"
		print "Gives a REPL if no argument given."
		exit(1)

	if len(sys.argv) == 2:
		with open(sys.argv[1]) as f:
			source_code = f.read()
		bytecode = cl_compiler.source_to_bytecode(source_code, sys.argv[1])
		cl_execute_string(bytecode)
	else:
		print "Cl REPL v(no version number yet)"
		ctx = ClContext()
		while True:
			inp = raw_input(">>> ")
			if inp == ".":
				# We begin a multiline."
				lines = []
				while lines[-1:] != ["."]:
					lines.append(raw_input("... "))
				inp = "\n".join(lines)
			bytecode = cl_compiler.source_to_bytecode(inp, "<repl>")
			ctx.execute(bytecode)

