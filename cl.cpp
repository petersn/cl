// Some crappy language.

//#define DEBUG_OUTPUT

#include <unordered_set>
#include <iterator>
#include <iostream>
#include <istream>
#include <ostream>
#include <sstream>
#include <fstream>
#include "cl.h"

extern "C" {
#include <unistd.h>
#include <dlfcn.h>
#include <execinfo.h>
#include <signal.h>
}

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

	uint32_t temp_length;

#define READ_STRING(dest) \
	READ_INTO(temp_length, uint32_t); \
	dest = string(data, temp_length); \
	data += temp_length; \
	length -= temp_length;

	// First we read in the function name.
	READ_STRING(function_name);

	// Then we read in the source file for the function.
	READ_STRING(source_file_path);

	// Read in the number of arguments that the function expects.
	READ_INTO(function_argument_count, uint32_t);

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

ClInstructionSequence::~ClInstructionSequence() {
	// Recursively delete instruction sequences that might occur inside ClMakeFunctionDescriptors of our instructions.
	for (ClInstruction& instr : instructions)
		if (instr.make_function_descriptor.executable_content != nullptr)
			delete instr.make_function_descriptor.executable_content;
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
	// Build an index of which instruction indices are jumped to.
	unordered_set<int> jumped_to_addresses;
	for (auto& instr : instructions) {
		if (instr.opcode == OPCODE_INDEX("ITERATE") or
			instr.opcode == OPCODE_INDEX("JUMP") or
			instr.opcode == OPCODE_INDEX("JUMP_IF_TRUTHY") or
			instr.opcode == OPCODE_INDEX("JUMP_IF_FALSEY")) {
			jumped_to_addresses.insert(instr.args[0]);
		}
	}
	int index = -1;
	for (auto& instr : instructions) {
		index++;
		ClOpcodeDesc desc = cl_opcode_descs[instr.opcode];
		int just_this_line_indentation = indentation;
		if (jumped_to_addresses.count(index) != 0) {
			stringstream ss;
			ss << index << ":";
			os << ss.str();
			just_this_line_indentation -= ss.str().size();
		}
		for (int i = 0; i < just_this_line_indentation; i++)
			os << " ";
		os << desc.name;
		for (int i = 0; i < desc.argument_count; i++) {
			os << " ";
			os << instr.args[i];
		}
		// If the instruction is a MAKE_FUNCTION, then recursively print the contents of the function.
		if (instr.opcode == OPCODE_INDEX("MAKE_FUNCTION")) {
			os << " args=" << instr.make_function_descriptor.function_argument_count;
			os << " subscope=" << instr.make_function_descriptor.subscope_length;
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
//	void* backtrace_array[256];
//	size_t size = backtrace(backtrace_array, sizeof(backtrace_array) / sizeof(backtrace_array[0]));
//	backtrace_symbols_fd(backtrace_array, size, STDERR_FILENO);
	cerr << "Sending signal to self to allow debugging." << endl;
	raise(SIGUSR1);
	exit(1);
} 

ClContext::ClContext() {
	data_ctx = new ClDataContext();
	ClFunction* f;

#define MAKE_METHOD(kind, name, function_argument_count, function) \
	f = data_ctx->create_function( \
		function_argument_count, \
		true, \
		data_ctx->register_permanent_string(name), \
		nullptr \
	); \
	f->native_executable_content = function; \
	data_ctx->default_type_tables[kind][name] = f;

#define MAKE_MEMBER_FUNCTION(instance, name, function_argument_count, function, should_be_a_method) \
	f = data_ctx->create_function( \
		function_argument_count, \
		should_be_a_method, \
		data_ctx->register_permanent_string(name), \
		nullptr \
	); \
	f->native_executable_content = function; \
	instance->table[name] = f;

#define MAKE_GLOBAL(name, function_argument_count, function) \
	MAKE_MEMBER_FUNCTION(data_ctx->global_scope, name, function_argument_count, function, false)

#define SET_TYPE_STRING(kind, type_string) \
	s = data_ctx->create<ClString>(); \
	s->contents = type_string; \
	data_ctx->default_type_tables[kind]["type"] = s;

	// Populate the methods for the basic types.
	MAKE_METHOD(CL_NIL, "str", 0, cl_builtin_nil_to_string)
	MAKE_METHOD(CL_INT, "str", 0, cl_builtin_int_to_string)
	MAKE_METHOD(CL_BOOL, "str", 0, cl_builtin_bool_to_string)
	MAKE_METHOD(CL_LIST, "str", 0, cl_builtin_list_to_string)

	MAKE_METHOD(CL_LIST, "append", 1, cl_builtin_list_append)
	MAKE_METHOD(CL_LIST, "iter", 0, cl_builtin_list_iter)
//	MAKE_METHOD(CL_LIST, "sort", cl_builtin_list_sort)
//	MAKE_METHOD(CL_LIST, "copy", cl_builtin_list_copy)

	// Set the type name strings.
	ClString* s;
	SET_TYPE_STRING(CL_NIL, "nil")
	SET_TYPE_STRING(CL_INT, "int")
	SET_TYPE_STRING(CL_BOOL, "bool")
	SET_TYPE_STRING(CL_LIST, "list")
	SET_TYPE_STRING(CL_RECORD, "record")
	SET_TYPE_STRING(CL_DICT, "dict")
	SET_TYPE_STRING(CL_STRING, "string")
	SET_TYPE_STRING(CL_FUNCTION, "function")
	// This type string should be overridden for each class.
	SET_TYPE_STRING(CL_INSTANCE, "instance")

	// Populate the global scope with builtin functions.
	MAKE_GLOBAL("len", 1, cl_builtin_len)
	MAKE_GLOBAL("methodify", 1, cl_builtin_methodify)
	MAKE_GLOBAL("getparent", 1, cl_builtin_getparent)
	MAKE_GLOBAL("getkind", 1, cl_builtin_getkind)

	// Upto is a complicated construction.
	// Its "closed_this" points to an instance, which it returns children of.
	// This instance has a single "iter" method, that we extract.
	MAKE_GLOBAL("upto", 1, cl_builtin_upto)
	// Give upto a closure over an appropriate instance.
	ClInstance* upto_base = data_ctx->create<ClInstance>();
	f->closed_this = upto_base;
	MAKE_MEMBER_FUNCTION(upto_base, "iter", 0, cl_builtin_upto_base_iter, true)

	// Populate the global scope with builtin values.
	cl_store_to_object_table(data_ctx->global_scope, data_ctx->nil, "nil");
	cl_store_to_object_table(data_ctx->global_scope, data_ctx->static_booleans[0], "False");
	cl_store_to_object_table(data_ctx->global_scope, data_ctx->static_booleans[1], "True");
}

ClContext::~ClContext() {
	delete data_ctx;
	for (void* handle : dlopened_handles)
		dlclose(handle);
}

static ClObj* pop(vector<ClObj*>& stack) {
	if (stack.size() == 0)
		cl_crash("Stack underflow on pop.");
	auto obj = stack.back();
	stack.pop_back();
	return obj;
}

ClObj* ClContext::execute(const string* traceback_name, const string* source_file_path, ClObj* closed_this, ClRecord* scope, ClInstructionSequence* seq) {
#ifdef DEBUG_OUTPUT
	cout << "=== exec ===" << endl;
#endif

	// Begin by adding a traceback entry, if we're non-null.
	ClTracebackEntry* traceback_entry = nullptr;
	if (traceback_name != nullptr) {
		data_ctx->traceback.push_back(ClTracebackEntry({traceback_name, source_file_path, 0}));
		traceback_entry = &data_ctx->traceback.back();
	}

	// Main interpreter.
	vector<ClObj*> stack;
	unsigned int instruction_pointer = 0;
	while (instruction_pointer < seq->instructions.size()) {
		ClInstruction& instruction = seq->instructions[instruction_pointer++];
		if (traceback_entry != nullptr)
			traceback_entry->line_number = instruction.line_number;

#ifdef DEBUG_OUTPUT
		cout << "[" << instruction_pointer-1 << "] Executing: " << cl_opcode_descs[instruction.opcode].name;
		for (auto& p : stack)
			cout << ", " << *p;
//		cout << "     " << data_ctx->global_scope;
		cout << endl;

		size_t starting_stack_size = stack.size();
//		getchar();
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
			case OPCODE_INDEX("DUP"): {
				if (stack.size() == 0)
					cl_crash("Stack underflow on dup.");
				ClObj* obj = stack.back();
				obj->inc_ref();
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("SWAP"): {
				ClObj* obj1 = pop(stack);
				ClObj* obj2 = pop(stack);
				stack.push_back(obj1);
				stack.push_back(obj2);
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
				auto obj = data_ctx->create<ClInt>();
				obj->value = instruction.args[0];
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_LIST"): {
				auto obj = data_ctx->create<ClList>();
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_RECORD"): {
				auto obj = new ClRecord(instruction.args[0], instruction.args[1], data_ctx->nil);
				data_ctx->register_object(obj);
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_DICT"): {
				auto obj = data_ctx->create<ClDict>();
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_STRING"): {
				auto obj = data_ctx->create<ClString>();
				obj->contents = instruction.data_field;
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_FUNCTION"): {
				// To make a function we produce a new record for its scope.
				ClMakeFunctionDescriptor& desc = instruction.make_function_descriptor;
				auto obj = data_ctx->create_function(
					desc.function_argument_count,
					false,
					&desc.function_name,
					&desc.source_file_path
				);
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
			case OPCODE_INDEX("MAKE_INSTANCE"): {
				ClObj* obj = data_ctx->create<ClInstance>();
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("MAKE_INSTANCE_P"): {
				ClInstance* parent = assert_kind<ClInstance>(pop(stack));
				ClInstance* obj = data_ctx->create<ClInstance>();
				obj->scope_parent = parent;
				// We don't inc or dec the ref count on parent, because being refed in obj makes up for being popped off the stack.
				stack.push_back(obj);
				break;
			}
			case OPCODE_INDEX("CALL"): {
				// The instruction argument is the count of arguments that we are passing in.
				uint32_t function_argument_count = instruction.args[0];
				// We need the stack to have these arguments, plus one additional value for the function itself.
				if (stack.size() < function_argument_count + 1)
					cl_crash("Stack underflow on call.");
				ClObj*& supposed_function = stack[stack.size() - function_argument_count - 1];
				// The first function argument lives right after the supposed function object that we are calling.
				ClObj** first_function_argument = (&supposed_function) + 1;
//				// First we pop the function argument off the stack.
//				// We do not decrement the ref count yet, because this could cause it to be prematurely collected.
//				ClObj* function_argument = pop(stack);
//				// Now we pop the actual function off the stack.
//				// We will decrement the ref count on this function, but only once we're done evaluating everything.
//				ClObj* supposed_function = pop(stack);
				// Here we take advantage of the fact that vectors are backed by a contiguous array.
				// Thus, we pass a pointer to the first function argument,
				ClObj* return_value = cl_perform_function_call(this, supposed_function, function_argument_count, first_function_argument);
				if (return_value->kind == CL_STOP_ITERATION)
					cl_crash("Stop iteration outside of looping context.");
//				// Now that execution of the call is over, it is safe to decerement the ref count on the function and argument.
//				supposed_function->dec_ref();
//				function_argument->dec_ref();
				// TODO: Decrease stack size, and decrement reference counts.
				for (uint32_t i = 0; i < function_argument_count + 1; i++) {
					pop(stack)->dec_ref();
				}
				// We push the return value onto our stack and do NOT increment the reference count, because
				// the reference count was never decremented when the object was popped off the callee's stack.
				stack.push_back(return_value);
				break;
			}
			case OPCODE_INDEX("ITERATE"): {
				if (stack.size() == 0)
					cl_crash("Stack underflow on iterate.");
				ClObj* iterator = stack.back();
//				cout << "About to call." << endl;
				// The nil inc_ref/dec_ref pair here is probably not necessary, but makes us strictly follow our protocol.
				data_ctx->nil->inc_ref();
				ClObj* return_value = cl_perform_function_call(this, iterator, 0, nullptr);
				data_ctx->nil->dec_ref();
//				cout << "Got return value: " << return_value->_vptr << endl;
				// If the value is a ClStopIteration, then jump to the target.
				if (return_value->kind == CL_STOP_ITERATION) {
//					cout << "Stop iteration!" << endl;
					instruction_pointer = instruction.args[0];
					return_value->dec_ref();
				} else {
					// Otherwise, push the iteration value onto the stack.
					stack.push_back(return_value);
				}
//				cout << "About to be done." << endl;
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
					cl_crash("Stack underflow on primitive list append.");
				ClObj* possibly_list = stack.back();
				if (possibly_list->kind != CL_LIST)
					data_ctx->traceback_and_crash("Attempted to append onto non-list.");
				ClList* list = static_cast<ClList*>(possibly_list);
				list->contents.push_back(new_item);
				// There is no need to adjust reference counts, because new_item was moved off the stack and into a list.
				break;
			}
			case OPCODE_INDEX("DICT_ASSIGN"): {
				break;
			}
			case OPCODE_INDEX("GLOBAL_LOAD"): // Fallthrough!
			case OPCODE_INDEX("DOT_LOAD"): {
				ClObj* obj_to_load_from;
				// Specialize this instruction for GLOBAL_LOAD.
				if (instruction.opcode == OPCODE_INDEX("GLOBAL_LOAD"))
					obj_to_load_from = data_ctx->global_scope;
				else
					obj_to_load_from = pop(stack);
				ClObj* result = cl_lookup_in_object_table(obj_to_load_from, instruction.data_field, true);
				// If the resultant object is a method, then bind it.
				// XXX: TODO: Check ref counting.
/*
				if (result->kind == CL_FUNCTION and static_cast<ClFunction*>(result)->is_method) {
					ClFunction* new_result = static_cast<ClFunction*>(result)->produce_bound_method(obj_to_load_from);
					data_ctx->register_object(new_result);
					// We now decrement the reference on ``result'', because we're not storing it anywhere,
					// and cl_lookup_in_object_table incremented its ref_count, assuming we would.
					result->dec_ref();
					result = new_result;
				}
*/
				// We don't need to increment result's ref count, because cl_lookup_in_object_table does it for us.
				// Here we dec ref only if we popped the object loaded from off the stack.
				if (instruction.opcode != OPCODE_INDEX("GLOBAL_LOAD"))
					obj_to_load_from->dec_ref();
				stack.push_back(result);
				break;
			}
			case OPCODE_INDEX("GLOBAL_STORE"): // Fallthrough!
			case OPCODE_INDEX("DOT_STORE"): {
				ClObj* obj_to_store_in;
				// Specialize this instruction for GLOBAL_STORE.
				if (instruction.opcode == OPCODE_INDEX("GLOBAL_STORE"))
					obj_to_store_in = data_ctx->global_scope;
				else
					obj_to_store_in = pop(stack);
				ClObj* value_to_store = pop(stack);
				cl_store_to_object_table(obj_to_store_in, value_to_store, instruction.data_field);
				value_to_store->dec_ref();
				// Here we dec ref only if we popped the stored-into object off the stack.
				if (instruction.opcode != OPCODE_INDEX("GLOBAL_STORE"))
					obj_to_store_in->dec_ref();
				break;
			}
			case OPCODE_INDEX("GET_GLOBAL"): {
				data_ctx->global_scope->inc_ref();
				stack.push_back(data_ctx->global_scope);
				break;
			}
			case OPCODE_INDEX("SET_GLOBAL"): {
				data_ctx->global_scope->dec_ref();
				ClInstance* new_scope = assert_kind<ClInstance>(pop(stack));
				data_ctx->global_scope = new_scope;
				// Being popped makes up for the new ref, so don't change the ref count on new_scope.
				break;
			}
			case OPCODE_INDEX("GET_THIS"): {
				ClObj* to_push = closed_this == nullptr ? data_ctx->nil : closed_this;
				to_push->inc_ref();
				stack.push_back(to_push);
				break;
			}
			case OPCODE_INDEX("UNARY_MINUS"): {
				ClObj* arg = pop(stack);
				ClObj* result = unary_minus(arg);
				arg->dec_ref();
				stack.push_back(result);
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
			case OPCODE_INDEX("STORE_INDEX"): {
				ClObj* index_value = pop(stack);
				ClObj* indexed_obj = pop(stack);
				ClObj* stored_obj = pop(stack);
				cl_store_by_index(indexed_obj, index_value, stored_obj);
				index_value->dec_ref();
				indexed_obj->dec_ref();
				stored_obj->dec_ref();
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
			case OPCODE_INDEX("TRACEBACK"): {
				data_ctx->traceback_and_crash(instruction.data_field);
				break;
			}
			default:
				cl_crash("BUG BUG BUG: Unhandled opcode in execute.");
		}

#ifdef DEBUG_OUTPUT
		int declared_stack_delta = cl_opcode_descs[instruction.opcode].stack_delta;
		// Here we skip the check if declared_stack_delta is -99.
		// This is to accomodate ITERATE, which currently violates the fixed stack delta doctrine.
		// While I figure out what I want to do about that, this is how it'll be.
		if (stack.size() != starting_stack_size + declared_stack_delta and declared_stack_delta != -99) {
			cout << cl_opcode_descs[instruction.opcode].name << endl;
			cout << "Bad stack size: " << stack.size() << " Old: " << starting_stack_size << " Delta: " << declared_stack_delta << endl;
			cl_crash("DEBUG ERROR");
		}
#endif
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

	if (traceback_name != nullptr)
		data_ctx->traceback.pop_back();

	// Return the top-of-stack (or nil) value.
	// The caller shouldn't increment the reference on this object when it inserts it somewhere, because
	// its ref count is already one high from the fact that we exempted it from the above decrementation.
	return return_value;
}

template <typename T>
static T dlsym_check(void* handle, const char* name) {
	dlerror();
	void* symbol = dlsym(handle, name);
	char* error = dlerror();
	if (error != nullptr) {
		string error_message = "couldn't load symbol ";
		error_message += name;
		error_message += ": ";
		error_message += error;
		cl_crash(error_message);
	}
	return reinterpret_cast<T>(symbol);
}

void ClContext::load_from_shared_object(string path, ClInstance* load_into_here) {
	// First we dlopen the given SO.
	void* handle = dlopen(path.c_str(), RTLD_LAZY);
	dlopened_handles.push_back(handle);

	if (handle == nullptr) {
		string error_message = "dlopen gave error: ";
		error_message += dlerror();
		cl_crash(error_message);
	}

	// Find the main table.
	ClSOEntry* cl_module_table = dlsym_check<ClSOEntry*>(handle, "cl_module_table");

	// Pull out all the symbols.
	int index = 0;
	while (true) {
		ClSOEntry& entry = cl_module_table[index++];
		// The table is ended by a sentinel entry with null name.
		if (entry.name == nullptr)
			break;

		cout << "Loading " << entry.name << endl;

		// If we're creating a new object, then we need to dec_ref after assigning into
		// load_into_here's object table, because otherwise the ref count will be 2, not 1.
		bool new_object_that_needs_dec_ref = false;
		ClObj* obj;
		switch (entry.kind) {
			case CL_NIL: {
				obj = data_ctx->nil;
				break;
			}
			case CL_INT: {
				ClInt* _obj = data_ctx->create<ClInt>();
				obj = _obj;
				_obj->value = (cl_int_t) entry.value;
				new_object_that_needs_dec_ref = true;
				break;
			}
			case CL_BOOL: {
				obj = data_ctx->static_booleans[entry.value != 0];
				break;
			}
			case CL_STRING: {
				ClString* _obj = data_ctx->create<ClString>();
				obj = _obj;
				_obj->contents = (const char*) entry.value;
				new_object_that_needs_dec_ref = true;
				break;
			}
			// XXX: TODO: Handle CL_FUNCTION entries appropriately.
/*
			case CL_FUNCTION: {
				ClFunction* _obj = data_ctx->create<ClFunction>();
				obj = _obj;
				_obj->native_executable_content = (ClObj* (*)(ClFunction* this_function, int argument_count, ClObj** arguments)) entry.value;
				new_object_that_needs_dec_ref = true;
				break;
			}
*/
			default:
				cl_crash("Bad kind in cl_module_table in shared object.");
		}
		cl_store_to_object_table(load_into_here, obj, entry.name);
		if (new_object_that_needs_dec_ref)
			obj->dec_ref();
	}

	// Find an initialization routine.
	void (*cl_module_init)(ClContext* ctx, ClInstance* load_into_here) =
	             (void (*)(ClContext* ctx, ClInstance* load_into_here)) dlsym(handle, "cl_module_init");
	if (cl_module_init != nullptr) {
		cout << "Calling module init." << endl;
		cl_module_init(this, load_into_here);
	}

	// Run some bytecode if requested.
	char* cl_module_init_bytecode = (char*) dlsym(handle, "cl_module_init_bytecode");
	if (cl_module_init_bytecode != nullptr) {
		int* cl_module_init_bytecode_length = (int*) dlsym(handle, "cl_module_init_bytecode_length");
		if (cl_module_init_bytecode_length == nullptr) {
			cout << "No symbol cl_module_init_bytecode_length in shared object." << endl;
			cout << "Add the following line to your source:" << endl;
			cout << "  int cl_module_init_bytecode_length = sizeof(cl_module_init_bytecode) - 1;";
			cl_crash("See above.");
		}
		string bytecode(cl_module_init_bytecode, *cl_module_init_bytecode_length);
		auto program = ClInstructionSequence::decode_opcodes(bytecode);
		cout << *program << endl;
		auto root_scope = new ClRecord(0, 0, data_ctx->nil);
		string traceback_name = "<";
		traceback_name += path;
		traceback_name += " embedded bytecode>";
		string source_file_path = "<sourceless>";
		ClObj* return_value = execute(&traceback_name, &source_file_path, nullptr, root_scope, program);
		return_value->dec_ref();
		delete root_scope;
		delete program;
	}
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
	cout << *program;

	auto ctx = new ClContext();
	auto root_scope = new ClRecord(0, 0, ctx->data_ctx->nil);

//	ctx->load_from_shared_object("./stdlib.so", ctx->data_ctx->global_scope);

	// Execute the program!
	ClObj* return_value = ctx->execute(nullptr, nullptr, nullptr, root_scope, program);
	// Decrement the ref count so this last return value gets reaped.
	return_value->dec_ref();

	// Unref all permanent objects.
	// At this point all objects should have a ref count of zero, or be in unreachable cycles.
	ctx->data_ctx->unref_all_permanent_objects();

	// Check for leaked objects.
	if (ctx->data_ctx->objects.size() > 0) {
		cout << "WARNING: Leaked objects:" << endl;
		for (ClObj* obj : ctx->data_ctx->objects) {
			cout << "    (" << obj << ") " << *obj << endl;
		}
	}

	cout << "Freed: " << ctx->data_ctx->objects_freed << endl;

	delete root_scope;
	delete ctx;
	delete program;
}

extern "C" void* cl_make_context(void) {
	return new ClContext();
}

extern "C" void cl_execute_string_in_context(void* context, const char* input, int length) {
	ClContext* ctx = static_cast<ClContext*>(context);
	string s(input, length);
	auto program = ClInstructionSequence::decode_opcodes(s);

	auto root_scope = new ClRecord(0, 0, ctx->data_ctx->nil);

//	ctx->load_from_shared_object("./stdlib.so", ctx->data_ctx->global_scope);

	// Execute the program!
	ClObj* return_value = ctx->execute(nullptr, nullptr, nullptr, root_scope, program);
	// Decrement the ref count so this last return value gets reaped.
	return_value->dec_ref();

	delete root_scope;
	// XXX: Use after free from this.
	delete program;
}

extern "C" void cl_free_context(void* context) {
	ClContext* ctx = static_cast<ClContext*>(context);
	ctx->data_ctx->unref_all_permanent_objects();
	delete ctx;
}

