// This is part of the standard library.

#include "cl.h"
using namespace std;

extern "C" void cl_module_init(ClContext* ctx, ClInstance* load_into_here) {
	cout << "Standard initialization!" << endl;
}

extern "C" const char cl_module_init_bytecode[] = "\x0b>\x00\x00\x00\x06\x00\x00\x00<root>\x0c\x00\x00\x00<sourceless>\x01\x00\x00\x00\x00\x00\x00\x00!\x01\x00\x00\x00\x00\x00\x00\x00\n\r\x00\x00\x00Hello, world! \x05\x0c";
int cl_module_init_bytecode_length = sizeof(cl_module_init_bytecode) - 1;

ClSOEntry cl_module_table[] = {
	{"basic", CL_INT, (void*) 17},
	{nullptr, (ClKind) 0, nullptr},
};

