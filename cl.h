// Some crappy language.

#ifndef _CL_MAIN_HEADER_H
#define _CL_MAIN_HEADER_H

// Forward declaration for the cyclic include of structures.h.
struct ClInstructionSequence;
void cl_crash(std::string message) __attribute__ ((noreturn));

#include "structures.h"
#include <string.h>
#include <vector>
#include <string>

#define MAX_OPCODE_ARGUMENT_COUNT 4

struct ClOpcodeDesc {
	const char* name;
	int argument_count;
	int stack_delta;
	bool takes_data_field;
};

constexpr ClOpcodeDesc cl_opcode_descs[] = {
// Format: {Name, Argument count, Stack delta, Takes data field}
// "Argument count" is the number of constant integer arguments that appear in the bytecode
// stream, not the number of arguments read off the stack, or anything like that.
// "Stack delta" is the change in the stack size after executing this opcode.
// "Takes data field" is true if the opcode takes an arbitrary constant string in the stream.

// For the sake of keeping things unified, my Python assembler parses this file to get the opcodes.
// These tags facilitate that parsing.
// WARNING: Commenting out an opcode won't stop the Python assembler from thinking it's real!
// BEGIN-PY-PARSING
	ClOpcodeDesc({"HALT",           0,  0, false}),
	ClOpcodeDesc({"NOP",            0,  0, false}),
	ClOpcodeDesc({"POP",            0, -1, false}),
	ClOpcodeDesc({"LOAD",           1,  1, false}),
	ClOpcodeDesc({"STORE",          1, -1, false}),
	ClOpcodeDesc({"MAKE_NIL",       0,  1, false}),
	ClOpcodeDesc({"MAKE_INT",       1,  1, false}),
	ClOpcodeDesc({"MAKE_LIST",      0,  1, false}),
	ClOpcodeDesc({"MAKE_RECORD",    2,  1, false}),
	ClOpcodeDesc({"MAKE_MAP",       0,  1, false}),
	ClOpcodeDesc({"MAKE_STRING",    0,  1, true }),
	ClOpcodeDesc({"MAKE_FUNCTION",  0,  1, true }),
	ClOpcodeDesc({"CALL",           0, -1, false}),
	ClOpcodeDesc({"ITERATE",        1,  0, false}),
	ClOpcodeDesc({"STOP_ITERATION", 0,  0, false}),
	ClOpcodeDesc({"LIST_APPEND",    0, -1, false}),
	ClOpcodeDesc({"DOT_ACCESS",     0,  0, true }),
	ClOpcodeDesc({"BINARY_PLUS",    0, -1, false}),
	ClOpcodeDesc({"BINARY_MINUS",   0, -1, false}),
	ClOpcodeDesc({"BINARY_TIMES",   0, -1, false}),
	ClOpcodeDesc({"BINARY_DIVIDE",  0, -1, false}),
	ClOpcodeDesc({"BINARY_MODULO",  0, -1, false}),
	ClOpcodeDesc({"BINARY_INDEX",   0, -1, false}),
	ClOpcodeDesc({"BINARY_IN",      0, -1, false}),
	ClOpcodeDesc({"BINARY_COMPARE", 1, -1, false}),
	ClOpcodeDesc({"STORE_INDEX",    0, -2, false}),
	ClOpcodeDesc({"JUMP",           1,  0, false}),
	ClOpcodeDesc({"JUMP_IF_TRUTHY", 1, -1, false}),
	ClOpcodeDesc({"JUMP_IF_FALSEY", 1, -1, false}),
	ClOpcodeDesc({"RETURN",         0,  0, false}),
	ClOpcodeDesc({"PRINT",          0, -1, false}),
// END-PY-PARSING
};

enum ClComparisonType {
	CL_COMP_EQ,
	CL_COMP_NOT_EQ,
	CL_COMP_LESS_THAN,
	CL_COMP_GREATER_THAN,
	CL_COMP_LESS_THAN_OR_EQ,
	CL_COMP_GREATER_THAN_OR_EQ,
};

constexpr int OP_MAX_OPCODE = (sizeof(cl_opcode_descs) / sizeof(cl_opcode_descs[0]));

namespace cl_template_trickery {
	// Here we use constexpr to make a function that evaluates to the index of a particular opcode name at compile time.
	constexpr int opcode_index_by_name(const char* name, int search_index=0) {
		return
			// Check if the opcode at search_index has the right name.
			strcmp(name, cl_opcode_descs[search_index].name) == 0 ?
				// If so we return said index.
				search_index :
				// Otherwise, we check if we're done searching.
				(search_index <= OP_MAX_OPCODE ?
					// If not, recurse on the rest of the array.
					opcode_index_by_name(name, search_index+1) :
					// Otherwise, return a sentinel. TODO: Crash here at compile time.
					-1
				);
	}

	// We define a template that passes through its single template parameter, for constifying expressions.
	template <int x>
	struct useless_template {
		static constexpr int value = x;
	};
}

// This macro guarantees compile time evaluation of the above opcode_index_by_name function.
#define OPCODE_INDEX(name) (cl_template_trickery::useless_template<cl_template_trickery::opcode_index_by_name(name)>::value)

// This is a descriptor for the MAKE_FUNCTION opcode, and thus the name should be interpreted
// left associatively, that is, a ((make function) descriptor) not a (make (function descriptor)).
struct ClMakeFunctionDescriptor {
	uint32_t subscope_length;
	std::vector<std::pair<int, int>> subscope_closure_descriptor;
	ClInstructionSequence* executable_content;

	// This method decodes the descriptor from a string, and returns the number of bytes consumed.
	// One obvious protocol would be to pass in a string, but then we might take $O(n^2)$ time to
	// parse bytecode when functions are nested (and thus the descriptor contains another descriptor).
	// Thus, we instead take in a pointer to the bytecode, and a length.
	size_t read_from(const char* data, size_t length);
};

struct ClInstruction {
	int opcode;
	cl_int_t args[MAX_OPCODE_ARGUMENT_COUNT] = {0};
	std::string data_field;

	// These fields are specific to MAKE_FUNCTION calls.
	ClMakeFunctionDescriptor make_function_descriptor;
};

struct ClInstructionSequence {
	std::vector<ClInstruction> instructions;

	static ClInstructionSequence* decode_opcodes(const std::string& s);
	void pprint(std::ostream& os, int indentation=2) const;
};

std::ostream& operator << (std::ostream& os, const ClInstructionSequence& seq);

class ClContext {
public:
	ClDataContext* data_ctx;

	ClContext();
	ClObj* execute(ClRecord* scope, ClInstructionSequence* seq);

	// Operations.
	ClObj* binary_plus(ClObj* left, ClObj* right);
	ClObj* binary_minus(ClObj* left, ClObj* right);
	ClObj* binary_times(ClObj* left, ClObj* right);
	ClObj* binary_divide(ClObj* left, ClObj* right);
	ClObj* binary_modulo(ClObj* left, ClObj* right);
	ClObj* binary_index(ClObj* left, ClObj* right);
	ClObj* binary_in(ClObj* left, ClObj* right);
	ClObj* binary_compare(ClObj* left, ClObj* right, ClComparisonType comparison_type);
};

bool cl_coerce_to_boolean(ClObj* obj);
ClObj* cl_perform_function_call(ClContext* ctx, ClObj* supposed_function, ClObj* argument);
ClObj* cl_lookup_in_object_table(ClObj* object, const std::string& name);

ClObj* cl_builtin_nil_to_string(ClFunction* this_function, ClObj* argument);
ClObj* cl_builtin_int_to_string(ClFunction* this_function, ClObj* argument);
ClObj* cl_builtin_bool_to_string(ClFunction* this_function, ClObj* argument);
ClObj* cl_builtin_list_to_string(ClFunction* this_function, ClObj* argument);
ClObj* cl_builtin_list_append(ClFunction* this_function, ClObj* argument);
ClObj* cl_builtin_list_iter(ClFunction* this_function, ClObj* argument);

// Our C API.
extern "C" {

void cl_execute_string(const char* input, int length);

}

#endif

