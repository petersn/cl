// Some crappy language.

#include <iterator>
#include <iostream>
#include <istream>
#include <ostream>
#include <sstream>
#include <fstream>
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
			instr.make_function_descriptor.read_from(&instr.data_field[0], instr.data_field.length());
		}

		result->instructions.push_back(instr);
	}

	return result;
}

void ClInstructionSequence::pprint(ostream& os, int indentation) const {
//	if (indentation == 2)
//		os << "Instructions: (" << instructions.size() << ")" << endl;
	for (auto& instr : instructions) {
		ClOpcodeDesc desc = cl_opcode_descs[instr.opcode];
		for (int i = 0; i < indentation; i++)
			os << " ";
		os << desc.name;
		for (int i = 0; i < desc.argument_count; i++) {
			os << " ";
			os << instr.args[i];
		}
		// If the instruction is a MAKE_FUNCTION, then recursively print the contents of the function.
		if (instr.opcode == OPCODE_INDEX("MAKE_FUNCTION")) {
			os << " " << instr.make_function_descriptor.subscope_length;
			for (auto& p : instr.make_function_descriptor.subscope_closure_descriptor) {
				os << " " << p.first << "->" << p.second;
			}
			os << " {" << endl;
			instr.make_function_descriptor.executable_content->pprint(os, indentation+2);
			for (int i = 0; i < indentation; i++)
				os << " ";
			os << "}" << endl;
		} else {
			os << endl;
		}
	}
}

ostream& operator << (ostream& os, const ClInstructionSequence& seq) {
	seq.pprint(os);
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

ClObj* ClContext::execute(ClRecord* scope, ClInstructionSequence* seq) {
	cout << "=== exec ===" << endl;

	// Main interpreter.
	vector<ClObj*> stack;
	int instruction_pointer = 0;
	while (instruction_pointer < seq->instructions.size()) {
		ClInstruction& instruction = seq->instructions[instruction_pointer];
		instruction_pointer++;

		cout << "Executing: " << cl_opcode_descs[instruction.opcode].name;
		for (auto& p : stack)
			cout << ", " << *p;
		cout << endl;

		switch (instruction.opcode) {
			case OPCODE_INDEX("HALT"): {
				cl_crash("Halt!");
				break;
			}
			case OPCODE_INDEX("NOP"): {
				cout << "Uncollected objects:" << endl;
				for (ClObj* obj : data_ctx->objects) {
					cout << "    [" << obj->ref_count << "]  " << *obj << endl;
				}
				cout << endl;
				break;
			}
			case OPCODE_INDEX("POP"): {
				pop(stack)->dec_ref();
				break;
			}
			case OPCODE_INDEX("LOAD"): {
				ClObj* obj = scope->load(instruction.args[0]);
				obj->inc_ref();
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("STORE"): {
				ClObj* obj = pop(stack);
				scope->store(instruction.args[0], obj);
				obj->dec_ref();
				break;
			}
			case OPCODE_INDEX("MAKE_NIL"): {
				data_ctx->nil->inc_ref();
				stack.push_back(data_ctx->nil);
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
			case OPCODE_INDEX("MAKE_LIST"): {
				auto obj = new ClList();
				obj->ref_count = 1;
				data_ctx->register_object(obj);
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_RECORD"): {
				auto obj = new ClRecord(instruction.args[0], instruction.args[1], data_ctx->nil);
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

				// To make a function we produce a new record for its scope.
				ClMakeFunctionDescriptor& desc = instruction.make_function_descriptor;
				ClRecord* func_scope = new ClRecord(0, desc.subscope_length, data_ctx->nil);
				func_scope->ref_count = 1;
				data_ctx->register_object(func_scope);
				// We then copy into the scope as per our closure descriptor.
				for (auto& p : desc.subscope_closure_descriptor)
					func_scope->store(p.second, scope->load(p.first));
				// Finally, we bind this scope into our function.
				obj->executable_content = desc.executable_content;
				obj->closure = func_scope;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("CALL"): {
				// First we pop the function argument off the stack.
				// We do not decrement the ref count yet, because this could cause it to be prematurely collected.
				// However, once we insert into the child scope below it becomes safe to decrement.
				ClObj* function_argument = pop(stack);
				// Now we pop the actual function off the stack.
				// We will decrement the ref count on this function, but only once we're done evaluating everything.
				ClObj* possibly_function_obj = pop(stack);
				// Type check the object.
				if (possibly_function_obj->kind != CL_FUNCTION) {
					cout << "Trying to call this: (" << *possibly_function_obj << " " << *function_argument << ")" << endl;
					cout << "KIND: " << possibly_function_obj->kind << endl;
					cout << "KIND: " << function_argument->kind << endl;
					cl_crash("Attempted to call non-function.");
				}
				ClFunction* function_obj = static_cast<ClFunction*>(possibly_function_obj);
				// We duplicate the function closure to get a scope to execute in.
				// Note that we neither register nor set the ref count on this record, because we're about to throw it away.
				ClRecord* child_scope = function_obj->closure->duplicate();
				// We set the magic value 0 in the child_scope to be the passed in argument.
				child_scope->store(0, function_argument);
				// Now that this child scope has ownership over the argument it is safe to decrement our reference count.
				function_argument->dec_ref();
				// Do execution!
				ClObj* return_value = execute(child_scope, function_obj->executable_content);
				// By deleting the child scope we effectively decrement the ref count on function_argument, completing our obligation.
				delete child_scope;
				// Now that execution of the call is over, it is safe to decerement the ref count on function_obj.
				function_obj->dec_ref();
				// We push the return value onto our stack and do NOT increment the reference count, because
				// the reference count was never decremented when the object was popped off the callee's stack.
				stack.push_back(return_value);
				break;
			}
#define BINARY_OPERATION(name, func) \
			case OPCODE_INDEX(name): { \
				ClObj* right = pop(stack); \
				ClObj* left = pop(stack); \
				ClObj* result = func(left, right); \
				left->dec_ref(); \
				right->dec_ref(); \
				stack.push_back(result); \
				break; \
			}
			BINARY_OPERATION("BINARY_PLUS", binary_plus)
			BINARY_OPERATION("BINARY_MINUS", binary_minus)
			BINARY_OPERATION("BINARY_TIMES", binary_times)
			BINARY_OPERATION("BINARY_DIVIDE", binary_divide)
			BINARY_OPERATION("BINARY_MODULO", binary_modulo)
			BINARY_OPERATION("BINARY_INDEX", binary_index)
			BINARY_OPERATION("BINARY_IN", binary_in)
//			BINARY_OPERATION("BINARY_COMPARE", binary_compare)
			case OPCODE_INDEX("PRINT"): {
				ClObj* obj = pop(stack);
				cout << " ---> Printing: " << *obj << endl;
				obj->dec_ref();
				break;
			}
			default:
				cl_crash("BUG BUG BUG: Unhandled opcode in execute.");
		}
	}

	// Here we compute the return value.
	// Return the top of stack, or nil if the stack is empty.
	ClObj* return_value;
	if (stack.size() > 0) {
		return_value = pop(stack);
	} else {
		return_value = data_ctx->nil;
		// This increment is necessary -- the caller will insert our return value somewhere without
		// incrementing the reference count on the object we hand them, so this is correct.
		data_ctx->nil->inc_ref();
	}

	// Decrement references to all the remaining objects in the stack, as our stack is about to be reaped.
	for (auto& p : stack)
		p->dec_ref();

	// Return the top-of-stack (or nil) value.
	// The caller shouldn't increment the reference on this object when it inserts it somewhere, because
	// its ref count is already one high from the fact that we exempted it from the above decrementation.
	return return_value;
}

// Include the various operations and functions.
#include "operations.cpp"

string slurp_stream(ifstream& in) {
	stringstream sstr;
	sstr << in.rdbuf();
	return sstr.str();
}

int main(int argc, char** argv) {
	ifstream ifs("bytecode.cl", ios::in | ios::binary);
	string slurp = slurp_stream(ifs);

//	cin >> noskipws;
//	istream_iterator<char> it(cin);
//	istream_iterator<char> end;
//	string stdin_slurp(it, end);	

	cout << "Read in " << slurp.size() << " bytes of bytecode." << endl;

	auto program = ClInstructionSequence::decode_opcodes(slurp);
	cout << *program;

	auto ctx = new ClContext();
	auto root_scope = new ClRecord(0, 0, ctx->data_ctx->nil);

	// Execute the program!
	ClObj* return_value = ctx->execute(root_scope, program);
	// Decrement the ref count so this last return value gets reaped.
	return_value->dec_ref();

	// Check for leaked objects.
	if (ctx->data_ctx->objects.size() > 0) {
		cout << "WARNING: Leaked objects:" << endl;
		for (ClObj* obj : ctx->data_ctx->objects) {
			cout << "    " << *obj << endl;
		}
	}
	// Check nil's reference count.
	if (ctx->data_ctx->nil->ref_count != 1)
		cout << "WARNING: Ref count on nil not one: " << ctx->data_ctx->nil->ref_count << endl;
}

