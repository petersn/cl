// Some crappy language.

#ifndef _CL_MAIN_HEADER_H
#define _CL_MAIN_HEADER_H

#include <cstring>
#include <vector>
#include <string>

// Forward declaration for the cyclic include of structures.h.
struct ClInstructionSequence;
void cl_crash(std::string message) __attribute__ ((noreturn));

#include "structures.h"

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
	ClOpcodeDesc({"HALT",            0,  0, false}),
	ClOpcodeDesc({"NOP",             0,  0, false}),
	ClOpcodeDesc({"POP",             0, -1, false}),
	ClOpcodeDesc({"DUP",             0,  1, false}),
	ClOpcodeDesc({"SWAP",            0,  0, false}),
	ClOpcodeDesc({"LOAD",            1,  1, false}),
	ClOpcodeDesc({"STORE",           1, -1, false}),
	ClOpcodeDesc({"MAKE_NIL",        0,  1, false}),
	ClOpcodeDesc({"MAKE_INT",        1,  1, false}),
	ClOpcodeDesc({"MAKE_LIST",       0,  1, false}),
	ClOpcodeDesc({"MAKE_RECORD",     2,  1, false}),
	ClOpcodeDesc({"MAKE_DICT",       0,  1, false}),
	ClOpcodeDesc({"MAKE_STRING",     0,  1, true }),
	ClOpcodeDesc({"MAKE_FUNCTION",   0,  1, true }),
	ClOpcodeDesc({"MAKE_INSTANCE",   0,  1, false}),
	ClOpcodeDesc({"MAKE_INSTANCE_P", 0,  0, false}),
	ClOpcodeDesc({"CALL",            1,-99, false}),
	// ITERATE has a stack delta of 0 if iteration is over, and +1 otherwise. :/
	// I haven't yet figured out what I want to do about this.
	// For now I set it to -99, to indicate that no value is correct.
	ClOpcodeDesc({"ITERATE",         1,-99, false}),
	ClOpcodeDesc({"STOP_ITERATION",  0,  0, false}),
	ClOpcodeDesc({"LIST_APPEND",     0, -1, false}),
	ClOpcodeDesc({"DICT_ASSIGN",     0, -2, false}),
	ClOpcodeDesc({"DOT_LOAD",        0,  0, true }),
	ClOpcodeDesc({"DOT_STORE",       0, -2, true }),
	// Here GLOBAL_LOAD and GLOBAL_STORE are for efficiency.
	// They're equivalent to GET_GLOBAL followed by a LOAD/STORE.
	ClOpcodeDesc({"GLOBAL_LOAD",     0,  1, true }),
	ClOpcodeDesc({"GLOBAL_STORE",    0, -1, true }),
	ClOpcodeDesc({"GET_GLOBAL",      0,  1, false}),
	ClOpcodeDesc({"SET_GLOBAL",      0, -1, false}),
	ClOpcodeDesc({"GET_THIS",        0,  1, false}),
	ClOpcodeDesc({"UNARY_NOT",       0,  0, false}),
	ClOpcodeDesc({"UNARY_MINUS",     0,  0, false}),
	ClOpcodeDesc({"BINARY_PLUS",     0, -1, false}),
	ClOpcodeDesc({"BINARY_MINUS",    0, -1, false}),
	ClOpcodeDesc({"BINARY_TIMES",    0, -1, false}),
	ClOpcodeDesc({"BINARY_DIVIDE",   0, -1, false}),
	ClOpcodeDesc({"BINARY_MODULO",   0, -1, false}),
	ClOpcodeDesc({"BINARY_INDEX",    0, -1, false}),
	ClOpcodeDesc({"BINARY_IN",       0, -1, false}),
	ClOpcodeDesc({"BINARY_AND",      0, -1, false}),
	ClOpcodeDesc({"BINARY_OR",       0, -1, false}),
	ClOpcodeDesc({"BINARY_COMPARE",  1, -1, false}),
	ClOpcodeDesc({"STORE_INDEX",     0, -3, false}),
	ClOpcodeDesc({"JUMP",            1,  0, false}),
	ClOpcodeDesc({"JUMP_IF_TRUTHY",  1, -1, false}),
	ClOpcodeDesc({"JUMP_IF_FALSEY",  1, -1, false}),
	ClOpcodeDesc({"RETURN",          0,  0, false}),
	ClOpcodeDesc({"PRINT",           0, -1, false}),
	ClOpcodeDesc({"TRACEBACK",       0,  0, true }),
// This is a pseudo-opcode that exists purely in the input opcode stream.
// It should be removed from the opcode stream before exection.
	ClOpcodeDesc({"LINE_NUMBER",     1,-99, false}),
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
	uint32_t function_argument_count = 0;
	uint32_t subscope_length = 0;
	std::vector<std::pair<int, int>> subscope_closure_descriptor;
	ClInstructionSequence* executable_content = nullptr;
	std::string function_name = "";
	std::string source_file_path = "";

	// This method decodes the descriptor from a string, and returns the number of bytes consumed.
	// One obvious protocol would be to pass in a string, but then we might take $O(n^2)$ time to
	// parse bytecode when functions are nested (and thus the descriptor contains another descriptor).
	// Thus, we instead take in a pointer to the bytecode, and a length.
	size_t read_from(const char* data, size_t length);
};

struct ClInstruction {
	int opcode = OPCODE_INDEX("HALT");
	cl_int_t args[MAX_OPCODE_ARGUMENT_COUNT] = {0};
	std::string data_field = "";
	int line_number = -1;

	// These fields are specific to MAKE_FUNCTION calls.
	ClMakeFunctionDescriptor make_function_descriptor;
};

struct ClInstructionSequence {
	std::vector<ClInstruction> instructions;

	~ClInstructionSequence();
	static ClInstructionSequence* decode_opcodes(const std::string& s);
	void pprint(std::ostream& os, int indentation=2) const;
};

std::ostream& operator << (std::ostream& os, const ClInstructionSequence& seq);

class ClContext {
public:
	ClDataContext* data_ctx;
	std::vector<void*> dlopened_handles;

	ClContext();
	~ClContext();
	ClObj* execute(const std::string* traceback_name, const std::string* source_file_path, ClObj* closed_this, ClRecord* scope, ClInstructionSequence* seq);
	void load_from_shared_object(std::string path, ClInstance* load_into_here);

	// Operations.
	ClObj* unary_not(ClObj* arg);
	ClObj* unary_minus(ClObj* arg);
	ClObj* binary_plus(ClObj* left, ClObj* right);
	ClObj* binary_minus(ClObj* left, ClObj* right);
	ClObj* binary_times(ClObj* left, ClObj* right);
	ClObj* binary_divide(ClObj* left, ClObj* right);
	ClObj* binary_modulo(ClObj* left, ClObj* right);
	ClObj* binary_index(ClObj* left, ClObj* right);
	bool raw_binary_in(ClObj* left, ClObj* right);
	ClObj* binary_in(ClObj* left, ClObj* right);
	ClObj* binary_and(ClObj* left, ClObj* right);
	ClObj* binary_or(ClObj* left, ClObj* right);
	bool raw_binary_compare(ClObj* left, ClObj* right, ClComparisonType comparison_type);
	ClObj* binary_compare(ClObj* left, ClObj* right, ClComparisonType comparison_type);

};

bool cl_coerce_to_boolean(ClObj* obj);
ClObj* cl_perform_function_call(ClContext* ctx, ClObj* supposed_function, int argument_count, ClObj** arguments);
ClObj* cl_lookup_in_object_table(ClObj* object, const std::string& name, bool bind_methods);
void cl_store_to_object_table(ClObj* object_to_store_in, ClObj* value_to_store, const std::string& name);
void cl_store_by_index(ClContext* ctx, ClObj* indexed_obj, ClObj* index_value, ClObj* stored_obj);

ClObj* cl_builtin_nil_to_string(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_int_to_string(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_bool_to_string(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_list_to_string(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_list_append(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_list_iter(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_list_iterator(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_len(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_methodify(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_getparent(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_getkind(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_upto(ClFunction* this_function, int argument_count, ClObj** arguments);
ClObj* cl_builtin_upto_base_iter(ClFunction* this_function, int argument_count, ClObj** arguments);

// Our C API.
extern "C" {

void cl_execute_string(const char* input, int length);
void* cl_make_context(void);
void cl_execute_string_in_context(void* context, const char* input, int length);
void cl_free_context(void* context);

// This API is used for modules.

typedef struct {
	const char* name;
	ClKind kind;
	void* value;
} ClSOEntry;

}

#endif

