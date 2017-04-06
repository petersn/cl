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

	// First we read out the function name size.
	uint32_t function_name_length;
	READ_INTO(function_name_length, uint32_t);
	// Then we read the actual data in.
	function_name = string(data, function_name_length);
	data += function_name_length;
	length -= function_name_length;

	// Next we read out the subscope size.
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
	int line_number = -1;

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
		instr.line_number = line_number;
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

		// If it's a LINE_NUMBER opcode, then it's a pseudo-instruction, and doesn't
		// actually get included in the executable stream.
		if (instr.opcode == OPCODE_INDEX("LINE_NUMBER")) {
			line_number = instr.args[0];
			continue;
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
	cerr << "Error: " << message << endl;
	exit(1);
} 

ClContext::ClContext() {
	data_ctx = new ClDataContext();
	ClFunction* f;

#define MAKE_METHOD(kind, name, function) \
	f = new ClFunction(); \
	f->function_name = data_ctx->register_permanent_string(name); \
	data_ctx->register_object(f); \
	f->is_method = true; \
	f->native_executable_content = function; \
	data_ctx->default_type_tables[kind][name] = f;

#define MAKE_GLOBAL(name, function) \
	f = new ClFunction(); \
	f->function_name = data_ctx->register_permanent_string(name); \
	data_ctx->register_object(f); \
	f->is_method = false; \
	f->native_executable_content = function; \
	data_ctx->global_scope->table[name] = f;

	// Populate the methods for the basic types.
	MAKE_METHOD(CL_NIL, "to_string", cl_builtin_nil_to_string)
	MAKE_METHOD(CL_INT, "to_string", cl_builtin_int_to_string)
	MAKE_METHOD(CL_BOOL, "to_string", cl_builtin_bool_to_string)
	MAKE_METHOD(CL_LIST, "to_string", cl_builtin_list_to_string)

	MAKE_METHOD(CL_LIST, "append", cl_builtin_list_append)
	MAKE_METHOD(CL_LIST, "iter", cl_builtin_list_iter)

	// Populate the global scope with builtin functions.
	MAKE_GLOBAL("len", cl_builtin_len)

	// Populate the global scope with builtin values.
	cl_store_to_object_table(data_ctx->global_scope, data_ctx->nil, "nil");
	cl_store_to_object_table(data_ctx->global_scope, data_ctx->static_booleans[0], "False");
	cl_store_to_object_table(data_ctx->global_scope, data_ctx->static_booleans[1], "True");
}

static ClObj* pop(vector<ClObj*>& stack) {
	if (stack.size() == 0)
		cl_crash("Stack underflow.");
	auto obj = stack.back();
	stack.pop_back();
	return obj;
}

ClObj* ClContext::execute(const string* traceback_name, ClRecord* scope, ClInstructionSequence* seq) {
#ifdef DEBUG_OUTPUT
	cout << "=== exec ===" << endl;
#endif

	// Begin by adding a traceback entry.
	data_ctx->traceback.push_back(ClTracebackEntry({traceback_name, new string("unknown file"), 42}));

	// Main interpreter.
	vector<ClObj*> stack;
	unsigned int instruction_pointer = 0;
	while (instruction_pointer < seq->instructions.size()) {
		ClInstruction& instruction = seq->instructions[instruction_pointer];
		instruction_pointer++;

#ifdef DEBUG_OUTPUT
		cout << "[" << instruction_pointer-1 << "] Executing: " << cl_opcode_descs[instruction.opcode].name;
		for (auto& p : stack)
			cout << ", " << *p;
		cout << endl;
#endif

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
				data_ctx->register_object(obj);

				obj->value = instruction.args[0];
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_LIST"): {
				auto obj = new ClList();
				data_ctx->register_object(obj);
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_RECORD"): {
				auto obj = new ClRecord(instruction.args[0], instruction.args[1], data_ctx->nil);
				data_ctx->register_object(obj);

				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_MAP"): {
				auto obj = new ClMap();
				data_ctx->register_object(obj);

				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_STRING"): {
				auto obj = new ClString();
				data_ctx->register_object(obj);

				obj->contents = instruction.data_field;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_FUNCTION"): {
				auto obj = new ClFunction();
				data_ctx->register_object(obj);

				// To make a function we produce a new record for its scope.
				ClMakeFunctionDescriptor& desc = instruction.make_function_descriptor;
				ClRecord* func_scope = new ClRecord(0, desc.subscope_length, data_ctx->nil);
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
				ClObj* function_argument = pop(stack);
				// Now we pop the actual function off the stack.
				// We will decrement the ref count on this function, but only once we're done evaluating everything.
				ClObj* supposed_function = pop(stack);
				ClObj* return_value = cl_perform_function_call(this, supposed_function, function_argument);
				if (return_value->kind == CL_STOP_ITERATION)
					cl_crash("Stop iteration outside of looping context.");
				// Now that execution of the call is over, it is safe to decerement the ref count on the function and argument.
				supposed_function->dec_ref();
				function_argument->dec_ref();
				// We push the return value onto our stack and do NOT increment the reference count, because
				// the reference count was never decremented when the object was popped off the callee's stack.
				stack.push_back(return_value);
				break;
			}
			case OPCODE_INDEX("ITERATE"): {
				if (stack.size() == 0)
					cl_crash("Stack underflow.");
				ClObj* iterator = stack.back();
				// The nil inc_ref/dec_ref pair here is probably not necessary, but makes us strictly follow our protocol.
				data_ctx->nil->inc_ref();
				ClObj* return_value = cl_perform_function_call(this, iterator, data_ctx->nil);
				data_ctx->nil->dec_ref();
				// If the value is a ClStopIteration, then jump to the target.
				if (return_value->kind == CL_STOP_ITERATION) {
					instruction_pointer = instruction.args[0];
					return_value->dec_ref();
				} else {
					// Otherwise, push the iteration value onto the stack.
					stack.push_back(return_value);
				}
				break;
			}
			case OPCODE_INDEX("STOP_ITERATION"): {
				data_ctx->stop_iteration->inc_ref();
				stack.push_back(data_ctx->stop_iteration);
				instruction_pointer = seq->instructions.size();
				break;
			}
			case OPCODE_INDEX("LIST_APPEND"): {
				ClObj* new_item = pop(stack);
				if (stack.size() == 0)
					cl_crash("Stack underflow.");
				ClObj* possibly_list = stack.back();
				if (possibly_list->kind != CL_LIST)
					data_ctx->traceback_and_crash("Attempted to append onto non-list.");
				ClList* list = static_cast<ClList*>(possibly_list);
				list->contents.push_back(new_item);
				// There is no need to adjust reference counts, because new_item was moved off the stack and into a list.
				break;
			}
			case OPCODE_INDEX("DOT_LOAD"): {
				ClObj* obj_to_load_from = pop(stack);
				ClObj* result = cl_lookup_in_object_table(obj_to_load_from, instruction.data_field);
				// If the resultant object is a method, then bind it.
				// XXX: TODO: Check ref counting.
				if (result->kind == CL_FUNCTION && static_cast<ClFunction*>(result)->is_method) {
					ClFunction* new_result = static_cast<ClFunction*>(result)->produce_bound_method(obj_to_load_from);
					data_ctx->register_object(new_result);
					// We now decrement the reference on ``result'', because we're not storing it anywhere,
					// and cl_lookup_in_object_table incremented its ref_count, assuming we would.
					result->dec_ref();
					result = new_result;
				}
				// We don't need to increment result's ref count, because cl_lookup_in_object_table does it for us.
				obj_to_load_from->dec_ref();
				stack.push_back(result);
				break;
			}
			case OPCODE_INDEX("DOT_STORE"): {
				ClObj* obj_to_store_in = pop(stack);
				ClObj* value_to_store = pop(stack);
				cl_store_to_object_table(obj_to_store_in, value_to_store, instruction.data_field);
				value_to_store->dec_ref();
				obj_to_store_in->dec_ref();
				break;
			}
			case OPCODE_INDEX("GET_GLOBAL"): {
				data_ctx->global_scope->inc_ref();
				stack.push_back(data_ctx->global_scope);
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
			case OPCODE_INDEX("BINARY_COMPARE"): {
				ClObj* right = pop(stack);
				ClObj* left = pop(stack);
				ClObj* result = binary_compare(left, right, (ClComparisonType)instruction.args[0]);
				left->dec_ref();
				right->dec_ref();
				stack.push_back(result);
				break;
			}
			case OPCODE_INDEX("JUMP"): {
				instruction_pointer = instruction.args[0];
				break;
			}
			case OPCODE_INDEX("JUMP_IF_TRUTHY"):
			case OPCODE_INDEX("JUMP_IF_FALSEY"): {
				ClObj* value = pop(stack);
				bool truth_value = cl_coerce_to_boolean(value);
				// Be careful not to decrement the reference until after we're done with the object.
				value->dec_ref();
				// Using != as an idiom for logical xor on bools.
				if (truth_value != (instruction.opcode == OPCODE_INDEX("JUMP_IF_FALSEY")))
					instruction_pointer = instruction.args[0];
				break;
			}
			case OPCODE_INDEX("RETURN"): {
				// Simply kill the execution of this function by setting the instruction pointer to the end of the sequence of instructions.
				instruction_pointer = seq->instructions.size();
				break;
			}
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

	data_ctx->traceback.pop_back();

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
	ifstream ifs("bytecode.clo", ios::in | ios::binary);
	string slurp = slurp_stream(ifs);
	cout << "Read in " << slurp.size() << " bytes of bytecode." << endl;

	cl_execute_string(slurp.c_str(), slurp.size());
	return 0;
}

extern "C" void cl_execute_string(const char* input, int length) {
	string s(input, length);
	auto program = ClInstructionSequence::decode_opcodes(s);
#ifdef DEBUG_OUTPUT
	cout << *program;
#endif

	auto ctx = new ClContext();
	auto root_scope = new ClRecord(0, 0, ctx->data_ctx->nil);

	// We come up with.
	string root_traceback_name = "<bug bug bug this should never show up>";

	// Execute the program!
	ClObj* return_value = ctx->execute(&root_traceback_name, root_scope, program);
	// Decrement the ref count so this last return value gets reaped.
	return_value->dec_ref();

	// Unref all permanent objects.
	// At this point all objects should have a ref count of zero, or be in unreachable cycles.
	ctx->data_ctx->unref_all_permanent_objects();

	// Check for leaked objects.
	if (ctx->data_ctx->objects.size() > 0) {
		cout << "WARNING: Leaked objects:" << endl;
		for (ClObj* obj : ctx->data_ctx->objects) {
			cout << "    " << *obj << endl;
		}
	}
}

