#!/usr/bin/python

import ctypes
import cl_compiler
import assemble

cl_shared_object = ctypes.CDLL("./cl.so")

_cl_execute_string = cl_shared_object.cl_execute_string
_cl_execute_string.argtypes = [ctypes.c_char_p, ctypes.c_int]
_cl_execute_string.restype = None

def cl_execute_string(s):
	_cl_execute_string(s, len(s))

if __name__ == "__main__":
	import sys

	if len(sys.argv) != 2:
		print "Usage: cl.py input.cl"
		exit(1)

	with open(sys.argv[1]) as f:
		source_code = f.read()

	bytecode = cl_compiler.source_to_bytecode(source_code)
	cl_execute_string(bytecode)

