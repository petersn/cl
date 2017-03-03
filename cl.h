// Some crappy language.

#ifndef _CL_MAIN_HEADER_H
#define _CL_MAIN_HEADER_H

#include "structures.h"
#include <string.h>
#include <vector>
#include <string>

#define MAX_OPCODE_ARGUMENT_COUNT 4

struct ClOpcodeDesc {
	const char* name;
	int argument_count;
};

constexpr ClOpcodeDesc cl_opcode_descs[] = {
// For the sake of keeping things unified, my Python assembler parses this file to get the opcodes.
// These tags facilitate that parsing.
// WARNING: Commenting out an opcode won't stop the Python assembler from thinking it's real!
// BEGIN-PY-PARSING
	ClOpcodeDesc({"HALT", 0}),
	ClOpcodeDesc({"POP", 0}),
	ClOpcodeDesc({"MAKE_INT", 1}),
	ClOpcodeDesc({"MAKE_CONS", 0}),
	ClOpcodeDesc({"MAKE_RECORD", 2}),
	ClOpcodeDesc({"MAKE_MAP", 0}),
	ClOpcodeDesc({"MAKE_STRING", 1}),
// END-PY-PARSING
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

struct ClInstruction {
	int opcode;
	cl_int_t args[MAX_OPCODE_ARGUMENT_COUNT] = {0};
};

struct ClInstructionSequence {
	std::vector<ClInstruction> instructions;

	static ClInstructionSequence* decode_opcodes(const std::string& s);
};

std::ostream& operator << (std::ostream& os, const ClInstructionSequence& seq);
void cl_crash(std::string message);

class ClContext {
	ClDataContext* data_ctx;

public:
	ClContext();
	void execute(ClInstructionSequence* seq);
};

#endif

