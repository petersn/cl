// Some crappy language.

#include <iterator>
#include <iostream>
#include <istream>
#include <ostream>
#include <unistd.h>
#include "cl.h"

using namespace std;

ClInstructionSequence* ClInstructionSequence::decode_opcodes(const string& s) {
	auto result = new ClInstructionSequence();

	// Decode the opcodes, one at a time.
	for (size_t string_index = 0; string_index < s.size(); string_index++) {
		unsigned char opcode_value = static_cast<unsigned char>(s[string_index]);

		if (opcode_value >= OP_MAX_OPCODE) {
			cl_crash("Invalid opcode.");
			// The above is no-return, but for completeness...
			return nullptr;
		}

		ClInstruction instr;
		instr.opcode = (int)opcode_value;
		ClOpcodeDesc desc = cl_opcode_descs[instr.opcode];

		// Decode arguments.
		for (int i = 0; i < desc.argument_count; i++) {
			if (string_index + 8 >= s.size())
				cl_crash("Opcode argument runs off end of instructions.");
			cl_int_t argument_value = *((cl_int_t*)&s[string_index+1]);
			instr.args[i] = argument_value;
			string_index += 8;
		}

		result->instructions.push_back(instr);
	}

	return result;
}

ostream& operator << (ostream& os, const ClInstructionSequence& seq) {
	os << "Instructions: (" << seq.instructions.size() << ")" << endl;
	for (auto& instr : seq.instructions) {
		ClOpcodeDesc desc = cl_opcode_descs[instr.opcode];
		os << "    " << desc.name;
		for (int i = 0; i < desc.argument_count; i++) {
			os << " ";
			os << instr.args[i];
		}
		os << endl;
	}
	return os;
}

void cl_crash(string message) {
	cerr << "ERROR: " << message << endl;
	exit(1);
} 

ClContext::ClContext() {
	data_ctx = new ClDataContext();
}

static ClObj* pop(vector<ClObj*>& stack) {
	if (stack.size() == 0)
		cl_crash("Stack underflow.");
	auto obj = stack.back();
	stack.pop_back();
	return obj;
}

void ClContext::execute(ClInstructionSequence* seq) {
	// Main interpreter.
	vector<ClObj*> stack;
	int instruction_pointer = 0;
	while (1) {
		if (instruction_pointer < 0 or instruction_pointer >= seq->instructions.size())
			cl_crash("Walked off end of program.");
		ClInstruction& instruction = seq->instructions[instruction_pointer];
		instruction_pointer++;

		switch (instruction.opcode) {
			case OPCODE_INDEX("HALT"): {
				cl_crash("Halt!");
				break;
			}
			case OPCODE_INDEX("POP"): {
				pop(stack)->dec_ref();
				break;
			}
			case OPCODE_INDEX("MAKE_INT"): {
				auto obj = new ClInt();
				obj->ref_count = 1;
				obj->value = instruction.args[0];
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_CONS"): {
				auto obj = new ClCons();
				obj->ref_count = 1;
				obj->head = pop(stack);
				ClObj* tail = pop(stack);
				// Here we do the conversion of nils into nullptrs.
				if (tail->kind == CL_NIL) {
					obj->tail = nullptr;
					// Make sure to count that this nil was popped off the stack!
					tail->dec_ref();
				} else if (tail->kind != CL_CONS) {
					cl_crash("Attempt to cons onto non-cons.");
				} else {
					obj->tail = static_cast<ClCons*>(tail);
				}
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_RECORD"): {
				auto obj = new ClRecord(data_ctx->nil, instruction.args[0], instruction.args[1]);
				obj->ref_count = 1;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_MAP"): {
				auto obj = new ClMap();
				obj->ref_count = 1;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_STRING"): {
				auto obj = new ClString();
				obj->ref_count = 1;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("UNARY_OP"): {
				break;
			}
			case OPCODE_INDEX("BINARY_OP"): {
				break;
			}
		}
	}
}

int main(int argc, char** argv) {
	cin >> noskipws;
	istream_iterator<char> it(cin);
	istream_iterator<char> end;
	string stdin_slurp(it, end);	

	auto program = ClInstructionSequence::decode_opcodes(stdin_slurp);
	cout << *program;

	auto ctx = new ClContext();
	ctx->execute(program);
}

