// Some crappy language.

#include <iterator>
#include <iostream>
#include <istream>
#include <ostream>
#include <unistd.h>
#include "cl.h"

using namespace std;

size_t ClMakeFunctionDescriptor::read_from(const char* data, size_t length) {
	size_t original_length = length;
#define READ_INTO(var, type) \
	do { \
		if (length < sizeof(type)) \
			cl_crash("MAKE_FUNCTION descriptor runs off end of instructions."); \
		var = *((type*)data); \
		data += sizeof(type); \
		length -= sizeof(type); \
	} while(0)

	// First we read out the subscope size.
	READ_INTO(subscope_length, uint32_t);
	// Read out the number of closure records.
	uint32_t closure_record_count;
	READ_INTO(closure_record_count, uint32_t);
	for (uint32_t i = 0; i < closure_record_count; i++) {
		// For each closure record we read the LOAD index, and STORE index.
		// This effects subscope[STORE] = outer_scope[LOAD] at runtime.
		uint32_t load_index, store_index;
		READ_INTO(load_index, uint32_t);
		READ_INTO(store_index, uint32_t);
		subscope_closure_descriptor.push_back(pair<int, int>(load_index, store_index));
	}

	// Finally, we decode the remaining instruction sequence.
	// XXX: TODO: Figure out what protocol I want here!
	// By copying this string I result in worst case $O(n^2)$ behavior.
	// Maybe switch to using string views?
	executable_content = ClInstructionSequence::decode_opcodes(string(data, length));

#undef READ_INTO

	// Return the total number of bytes consumed.
	return original_length - length;
}

ClInstructionSequence* ClInstructionSequence::decode_opcodes(const string& s) {
	auto result = new ClInstructionSequence();

	// Decode the opcodes, one at a time.
	for (size_t string_index = 0; string_index < s.size();) {
		unsigned char opcode_value = static_cast<unsigned char>(s[string_index]);

		// Increment the string index. We may increase it more for arguments/fields.
		string_index += 1;

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
			if (string_index + 8 > s.size())
				cl_crash("Opcode argument runs off end of instructions.");
			cl_int_t argument_value = *((cl_int_t*)&s[string_index]);
			string_index += 8;
			instr.args[i] = argument_value;
		}

		// Read a data field, if one is required.
		if (desc.takes_data_field) {
			if (string_index + 4 > s.size())
				cl_crash("Opcode data field length runs off end of instructions.");
			uint32_t data_field_length = *((uint32_t*)&s[string_index]);
			string_index += 4;
			if (string_index + data_field_length > s.size())
				cl_crash("Opcode data field runs off end of instructions.");
			instr.data_field = string(&s[string_index], data_field_length);
			string_index += data_field_length;
		}

		// If it's a MAKE_FUNCTION opcode we separately decode the closure description.
		if (instr.opcode == OPCODE_INDEX("MAKE_FUNCTION")) {
//			const char* descriptor_str = &s[string_index];
//			size_t effective_length = s.size() - string_index;
//			size_t bytes_consumed = instr.make_function_descriptor.read_from(descriptor_str, effective_length);
//			string_index += bytes_consumed;
			instr.make_function_descriptor.read_from(&instr.data_field[0], instr.data_field.length());
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

void ClContext::execute(ClRecord* scope, ClInstructionSequence* seq) {
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
				data_ctx->register_object(obj);

				obj->value = instruction.args[0];
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_CONS"): {
				auto obj = new ClCons();
				obj->ref_count = 1;
				data_ctx->register_object(obj);

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
				data_ctx->register_object(obj);

				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_MAP"): {
				auto obj = new ClMap();
				obj->ref_count = 1;
				data_ctx->register_object(obj);

				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_STRING"): {
				auto obj = new ClString();
				obj->ref_count = 1;
				data_ctx->register_object(obj);

				obj->contents = instruction.data_field;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_FUNCTION"): {
				auto obj = new ClFunction();
				obj->ref_count = 1;
				data_ctx->register_object(obj);

//				obj->contents = instruction.data_field;
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
	auto root_scope = new ClRecord(ctx->data_ctx->nil, 0, 0);
	ctx->execute(root_scope, program);
}

