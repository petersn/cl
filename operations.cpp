// Various operations for Cl.
// WARNING: This file is intended to be included directly into cl.cpp, for optimization purposes.
// TODO: Maybe just use link-time optimization instead?

#define Type_Case(t1, t2) \
	if (_left->kind  == cl_template_trickery::get_kind<t1>::kind and \
	    _right->kind == cl_template_trickery::get_kind<t2>::kind) { \
		auto left = static_cast<t1*>(_left); \
		auto right = static_cast<t2*>(_right);

#define End_Case  \
		return obj; \
	}

#define End_Case_No_Return \
	}

#define Give(type) \
	auto obj = new type(); \
	data_ctx->register_object(obj);

#define Double_Dispatch(operator_name) \
	{ \
		ClObj* dd_result = nullptr; \
		if (double_dispatch(this, \
			"__" operator_name "__", \
			"__r" operator_name "__", \
			dd_result, _left, _right)) \
			return dd_result; \
	}

static inline cl_int_t util_length_wrap(ClDataContext* data_ctx, cl_int_t index, cl_int_t length) {
	if (index < 0)
		index += length;
	if (index >= length)
		data_ctx->traceback_and_crash("Index out of range.");
	return index;
}

static bool double_dispatch(ClContext* ctx, string method_name, string r_method_name, ClObj*& result, ClObj* left, ClObj* right) {
	ClObj* callable;
	ClObj* other;
	// First try to dispatch on the left.
	if (left->kind == CL_INSTANCE) {
		callable = cl_lookup_in_object_table(left, method_name, true);
		other = right;
	} else if (right->kind == CL_INSTANCE) {
		callable = cl_lookup_in_object_table(right, r_method_name, true);
		other = left;
	} else {
		// Couldn't double dispatch.
		return false;
	}
	result = cl_perform_function_call(ctx, callable, 1, &other);
	callable->dec_ref();
	return true;
}

ClObj* ClContext::unary_not(ClObj* _arg) {
	bool truth_value = not cl_coerce_to_boolean(_arg);
	ClObj* obj = data_ctx->static_booleans[truth_value];
	obj->inc_ref();
	return obj;
}

ClObj* ClContext::unary_minus(ClObj* _arg) {
	ClInt* arg = assert_kind<ClInt>(_arg);
	ClInt* result = arg->parent->create<ClInt>();
	result->value = -arg->value;
	return result;
}

ClObj* ClContext::binary_plus(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value + right->value;
	End_Case

	Type_Case(ClString, ClString)
		Give(ClString)
		obj->contents = left->contents + right->contents;
	End_Case

	Type_Case(ClList, ClList)
		Give(ClList)
		obj->contents = left->contents;
		obj->contents.insert(obj->contents.end(), right->contents.begin(), right->contents.end());
		// We now increment every reference.
		for (ClObj* p : obj->contents)
			p->inc_ref();
	End_Case

	Double_Dispatch("plus")

	data_ctx->traceback_and_crash("Type error on binary plus.");
}

ClObj* ClContext::binary_minus(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value - right->value;
	End_Case

	Double_Dispatch("minus")

	data_ctx->traceback_and_crash("Type error on binary minus.");
}

ClObj* ClContext::binary_times(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value * right->value;
	End_Case

	Type_Case(ClInt, ClString)
		Give(ClString)
		// TODO: Replace with .reserve implementation.
    	ostringstream string_stream;
		for (cl_int_t i = 0; i < left->value; i++)
			string_stream << right->contents;
		obj->contents = string_stream.str();
	End_Case

	Type_Case(ClInt, ClList)
		Give(ClList)
		// TODO: Replace with .reserve implementation.
		for (cl_int_t i = 0; i < left->value; i++)
			obj->contents.insert(obj->contents.end(), right->contents.begin(), right->contents.end());
		for (ClObj* inserted : obj->contents)
			inserted->inc_ref();
	End_Case

	// In case of string * int or list * int, just swap the order around.
	if ((_left->kind == CL_STRING or _left->kind == CL_LIST) and _right->kind == CL_INT) {
		return binary_times(_right, _left);
	}

	Double_Dispatch("times")

	data_ctx->traceback_and_crash("Type error on binary times.");
}

ClObj* ClContext::binary_divide(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value / right->value;
	End_Case

	Double_Dispatch("divide")

	data_ctx->traceback_and_crash("Type error on binary divide.");
}

ClObj* ClContext::binary_modulo(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value % right->value;
		// Force the representative to be in [0, right).
		if (obj->value < 0)
			obj->value += right->value;
	End_Case

	Double_Dispatch("modulo")

	data_ctx->traceback_and_crash("Type error on binary modulo.");
}

ClObj* ClContext::binary_index(ClObj* _left, ClObj* _right) {
	Type_Case(ClString, ClInt)
		Give(ClString)
		cl_int_t index = util_length_wrap(data_ctx, right->value, left->contents.size());
		obj->contents = string(&left->contents[index], 1);
	End_Case

	Type_Case(ClList, ClInt)
		cl_int_t index = util_length_wrap(data_ctx, right->value, left->contents.size());
		ClObj* obj = left->contents[index];
		obj->inc_ref();
	End_Case

	if (_left->kind == CL_DICT) {
		ClObj* result = static_cast<ClDict*>(_left)->lookup(_right);
		result->inc_ref();
		return result;
	}

	// Attempt double dispatch.
	Double_Dispatch("index")

	data_ctx->traceback_and_crash("Type error on binary index.");
}

bool ClContext::raw_binary_in(ClObj* _left, ClObj* _right) {
	if (_right->kind == CL_LIST) {
		for (ClObj* elem : assert_kind<ClList>(_right)->contents) {
			if (raw_binary_compare(_left, elem, CL_COMP_EQ))
				return true;
		}
		return false;
	}

	if (_right->kind == CL_DICT) {
		DictHashEntry entry({_left});
		const auto& mapping = assert_kind<ClDict>(_right)->mapping;
		return mapping.find(entry) != mapping.end();
	}

	Double_Dispatch("in")

	// TODO: Add dispatch by iterating over the right argument.

	data_ctx->traceback_and_crash("Type error on binary in.");
}

ClObj* ClContext::binary_in(ClObj* _left, ClObj* _right) {
	bool truth_value = raw_binary_in(_left, _right);
	ClObj* obj = data_ctx->static_booleans[truth_value];
	obj->inc_ref();
	return obj;
}


// Maps a comparison result (from, e.g., memcmp) and a comparison type to an actual resulting truth value.
static bool comparison_to_truth_value(int comparison, ClComparisonType comparison_type) {
	switch (comparison_type) {
		case (CL_COMP_EQ):                 return comparison == 0;
		case (CL_COMP_NOT_EQ):             return comparison != 0;
		case (CL_COMP_LESS_THAN):          return comparison <  0;
		case (CL_COMP_GREATER_THAN):       return comparison >  0;
		case (CL_COMP_LESS_THAN_OR_EQ):    return comparison <= 0;
		case (CL_COMP_GREATER_THAN_OR_EQ): return comparison >= 0;
		default: cl_crash("BUG BUG BUG: Bad comparison type.");
	}
}

bool ClContext::raw_binary_compare(ClObj* _left, ClObj* _right, ClComparisonType comparison_type) {
	Type_Case(ClInt, ClInt)
		int comparison_result =
			left->value == right->value ? 0 :
			(left->value < right->value ? -1 : 1);
		return comparison_to_truth_value(comparison_result, comparison_type);
	End_Case_No_Return

	// XXX: TODO:
	// Implement appropriation comparisons here for the built in types, and delegate to a method for instances.

	Type_Case(ClString, ClString)
		// Compare the string contents in asciibetical order as byte strings.
		// TODO: Replace with appropriate std::min or something, when I have a network connection.
		size_t left_length = left->contents.size();
		size_t right_length = right->contents.size();
		size_t minimum_length = left_length;
		if (right_length < minimum_length)
			minimum_length = right_length;
		int comparison_result = memcmp(left->contents.data(), right->contents.data(), minimum_length);
		// If the comparison yields equality, we break ties with overall string length.
		if (comparison_result == 0) {
			if (left_length < right_length)
				comparison_result = -1;
			else if (left_length > right_length)
				comparison_result = 1;
		}
		// Finally, interpret this comparison_result subject to the requested comparison.
		return comparison_to_truth_value(comparison_result, comparison_type);
	End_Case_No_Return

/*
	Type_Case(ClList, ClList)
		if (comparison_type != CL_COMP_EQ and comparison_type != CL_COMP_NOT_EQ)
			data_ctx->traceback_and_crash("Currently only == and != are supported between lists.");
		bool truth_value = false;
		if (left->contents.size() != right->contents.size()) {
			truth_value = false;
		} else {
			// Recursively compare the contents of the list.
			for (size_t i = 0; i < left->contents.size(); i++) {
			}
		}
	End_Case
*/

	// For generic comparisons we use some simple rules.
	if (comparison_type == CL_COMP_EQ or comparison_type == CL_COMP_NOT_EQ) {
		bool truth_value = false;
		// Two objects of different kinds are never equal.
		if (_left->kind != _right->kind) {
			truth_value = false;
		} else {
			// Two objects of the same kind are equal only when they are isqual.
			truth_value = _left == _right;
		}
		// If we are testing inequality, invert the result.
		if (comparison_type == CL_COMP_NOT_EQ)
			truth_value = not truth_value;
		return truth_value;
//		ClObj* obj = data_ctx->static_booleans[truth_value];
//		obj->inc_ref();
//		return obj;
	}

	data_ctx->traceback_and_crash("Type error on binary compare.");
}

ClObj* ClContext::binary_compare(ClObj* _left, ClObj* _right, ClComparisonType comparison_type) {
	bool truth_value = raw_binary_compare(_left, _right, comparison_type);
	ClObj* obj = data_ctx->static_booleans[truth_value];
	obj->inc_ref();
	return obj;
}

// ===== Major helpers =====

bool cl_coerce_to_boolean(ClObj* obj) {
	switch (obj->kind) {
		case (CL_NIL):
			return false;
		case (CL_INT):
			return static_cast<ClInt*>(obj)->value != 0;
		case (CL_BOOL):
			return static_cast<ClBool*>(obj)->truth_value;
		case (CL_LIST):
			return static_cast<ClList*>(obj)->contents.size() > 0;
		case (CL_RECORD):
		case (CL_DICT):
			return true;
		case (CL_STRING):
			return static_cast<ClString*>(obj)->contents.size() > 0;
		case (CL_FUNCTION):
			return true;
		case (CL_INSTANCE):
			return true;
		default:
			cl_crash("BUG BUG BUG: Unhandled case in cl_coerce_to_boolean.");
			return false; // Suppress compiler warning.
	}
}

ClObj* cl_perform_function_call(ClContext* ctx, ClObj* supposed_function, int argument_count, ClObj** arguments) {
	// If it's an instance, then calling is subclassing/instantiation.
	if (supposed_function->kind == CL_INSTANCE) {
		ClInstance* instance_obj = assert_kind<ClInstance>(supposed_function);
		ClInstance* result = supposed_function->parent->create<ClInstance>();
		result->scope_parent = instance_obj;
		instance_obj->inc_ref();
		// We now check if the instance has a construct method.
		if (instance_obj->table.find("construct") != instance_obj->table.end()) {
			ClObj* constructor = cl_lookup_in_object_table(instance_obj, "construct", true);
//			ClFunction* f = assert_kind<ClFunction>(constructor);
//			cout << f->argument_count << endl;
//			cout << f->is_method << endl;
//			cout << f->closed_this << endl;
			ClObj* ignored = cl_perform_function_call(ctx, constructor, argument_count, arguments);
			ignored->dec_ref();
			constructor->dec_ref();
		} else {
			// If we have no construct method then enforce that we have been called with no arguments.
			if (argument_count != 0)
				ctx->data_ctx->traceback_and_crash("Cannot pass arguments to instance with no construct method.");
		}
		// Otherwise, we call the call method.
		return result;
	}
	ClFunction* function_obj = assert_kind<ClFunction>(supposed_function);
	// Make sure we have the right argument count.
	// TODO: Implement auto-currying here.
	if (argument_count != function_obj->argument_count)
		ctx->data_ctx->traceback_and_crash("Bad argument count on function call.");
	ClObj* return_value;
	// We now do an important case check, to determine if function_obj is a native function, or Cl function.
	// If it's a Cl function, then function_obj->native_executable_content is nullptr.
	// If it's native, then function_obj->native_executable_content is a ClObj* (*)(ClObj* argument) pointing to the code.
	if (function_obj->native_executable_content != nullptr) {
		// === Native function call ===
		return_value = function_obj->native_executable_content(function_obj, argument_count, arguments);
	} else {
		if (function_obj->executable_content == nullptr)
			cl_crash("BUG BUG BUG: Somehow we're calling a function with null native_executable_content and null executable_content!");
		// === Cl (non-native) function call ===
		// We duplicate the function closure to get a scope to execute in.
		// Note that we neither register nor set the ref count on this record, because we're about to throw it away.
		ClRecord* child_scope = function_obj->closure->duplicate();
		// BAD COMMENT: // We set the magic value 0 in the child_scope to be the passed in argument.
		// We set the arguments in the child scope.
		for (int i = 0; i < argument_count; i++)
			child_scope->store(i, arguments[i]);
		// Do execution!
		return_value = ctx->execute(function_obj->function_name, function_obj->source_file_path, function_obj->closed_this, child_scope, function_obj->executable_content);
		// By deleting the child scope we effectively decrement the ref count on the arguments, completing our obligation.
		delete child_scope;
	}
	return return_value;
}

ClObj* cl_lookup_in_object_table(ClObj* object, const string& name, bool bind_methods) {
	const unordered_map<string, ClObj*>* object_table;
	// We need to find the appropriate object table.
	// Either we are a ClInstance, in which case we have a table, or we're any other type, in which case we use the statically allocated table for that object.
	if (object->kind == CL_INSTANCE)
		object_table = &static_cast<ClInstance*>(object)->table;
	else
		object_table = &object->parent->default_type_tables[object->kind];
	auto lookup_result = object_table->find(name);

	// We will now evaluate some complicated logic to determine the result of this lookup, and populate lookup_result_obj.
	ClObj* lookup_result_obj;
	// There are now three cases based on the lookup_result.
	// Either:
	//   1) The name wasn't found, but we're an instance with a non-null parent.
	//      In this case, we recursively lookup on our parent to set lookup_result_obj.
	//   2) The name wasn't found, but the above doesn't apply.
	//      In this case, we have an attribute not found error.
	//   3) Everything else. (Specifically, the name WAS found.)
	//      In this case, set lookup_result_obj based on the found object.
	// Once we have evaluated this logic, we proceed to method binding.

	// Delegate to the parent if we're looking up in an instance, and the field isn't found, and the parent is non-null.
	if (object->kind == CL_INSTANCE and
	    lookup_result == object_table->end() and
	    static_cast<ClInstance*>(object)->scope_parent != nullptr) {
		// Case 1: Recursive lookup.
		// Note that we MUST set bind_methods to false on this lookup, so that we don't end up accidentally returning something that is bound to our parent.
		lookup_result_obj = cl_lookup_in_object_table(static_cast<ClInstance*>(object)->scope_parent, name, false);
		// XXX: Think carefully about this dec ref that is to follow...
		// It's necessary because our protocol means that cl_lookup_in_object_table causes an increment, but we're not storing this object anywhere.
		// But what if looking up returns an object with ref count 1, somehow?
		// For example, it could do this if it generated an object dynamically, like a bound method, or with some __getattr__ style handler.
		// For now I put in an assertion, and check for this. I think my design here is fundamentally bad.
		// One solution would be to set a flag if this recursive case is hit, and do one more dec ref on this object later.
		if (lookup_result_obj->ref_count <= 1) {
			cl_crash("BUG BUG BUG: Read the comments near this line in operations.cpp -- recursive lookup is fundamentally implemented wrong!");
		}
		lookup_result_obj->dec_ref();
		
	} else if (lookup_result == object_table->end()) {
		// Case 2: Attribute not found error.
		string message = string("No attribute \"") + name + "\" found.";
		object->parent->traceback_and_crash(message);
	} else {
		// Case 3: Attribute found, normal lookup.
		// Otherwise, increment the reference count and return the object we got.
		lookup_result_obj = (*lookup_result).second;
	}
	// If we are to bind methods, and we're pulling out a method, then bind it.
	if (bind_methods and
		lookup_result_obj->kind == CL_FUNCTION and
		static_cast<ClFunction*>(lookup_result_obj)->is_method) {
		// Bind the method to ``object'', which is the object we pulled it from.
		ClFunction* bound_result = static_cast<ClFunction*>(lookup_result_obj)->produce_bound_method(object);
		return bound_result;
	}
	lookup_result_obj->inc_ref();
	return lookup_result_obj;
}

void cl_store_to_object_table(ClObj* object_to_store_in, ClObj* value_to_store, const string& name) {
	if (object_to_store_in->kind != CL_INSTANCE)
		object_to_store_in->parent->traceback_and_crash(string("Attempt to mutate table of non-instance of kind ") + cl_kind_to_name[object_to_store_in->kind]);
	unordered_map<string, ClObj*>& object_table = static_cast<ClInstance*>(object_to_store_in)->table;
	// Decrement the ref count on the old object if we're overwriting.
	auto result = object_table.find(name);
	if (result != object_table.end())
		(*result).second->dec_ref();
	// Now we are free to overwrite.
	object_table[name] = value_to_store;
	value_to_store->inc_ref();
}

void cl_store_by_index(ClContext* ctx, ClObj* _indexed_obj, ClObj* _index_value, ClObj* stored_obj) {
	// TODO: Add a slice type, and handle it for index_value.
	switch (_indexed_obj->kind) {
		case (CL_LIST): {
			ClList* indexed_obj = assert_kind<ClList>(_indexed_obj);
			ClInt* index_value = assert_kind<ClInt>(_index_value);
			cl_int_t index = util_length_wrap(_indexed_obj->parent, index_value->value, indexed_obj->contents.size());
			// Must in stored_obj ref first, in case we're replacing the object with itself.
			stored_obj->inc_ref();
			indexed_obj->contents[index]->dec_ref();
			indexed_obj->contents[index] = stored_obj;
			break;
		}
		case (CL_DICT): {
			assert_kind<ClDict>(_indexed_obj)->assign(_index_value, stored_obj);
			break;
		}
		case (CL_INSTANCE): {
			ClObj* callable = cl_lookup_in_object_table(_indexed_obj, "__set_index__", true);
			ClObj* args[2] = {_index_value, stored_obj};
			ClObj* ignored = cl_perform_function_call(ctx, callable, 2, args);
			callable->dec_ref();
			ignored->dec_ref();
			break;
		}
		default:
			_indexed_obj->parent->traceback_and_crash("Type error on store-by-index.");
	}
}

// ===== Built-in functions =====

#define ASSERT_ARGUMENT_COUNT(n) \
	if (argument_count != n) { \
		cl_crash("BUG BUG BUG: Bad argument count in native builtin! The checks in cl_perform_function_call should make this impossible."); \
	}

ClObj* cl_builtin_nil_to_string(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	ClDataContext* data_ctx = this_function->parent;
	ClString* result = data_ctx->create<ClString>();
	result->contents = "nil";
	return result;
}

ClObj* cl_builtin_int_to_string(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	ClDataContext* data_ctx = this_function->parent;
	ClInt* argument = assert_kind<ClInt>(this_function->closed_this);
	ClString* result = data_ctx->create<ClString>();
	result->contents = to_string(argument->value);
	return result;
}

ClObj* cl_builtin_bool_to_string(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	ClDataContext* data_ctx = this_function->parent;
	ClBool* argument = assert_kind<ClBool>(this_function->closed_this);
	ClString* result = data_ctx->create<ClString>();
	if (argument->truth_value)
		result->contents = "True";
	else
		result->contents = "False";
	return result;
}

ClObj* cl_builtin_list_to_string(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	ClDataContext* data_ctx = this_function->parent;
	ClList* argument = assert_kind<ClList>(this_function->closed_this);
	ClString* result = data_ctx->create<ClString>();
	stringstream ss;
	ss << "[";
	for (size_t i = 0; i < argument->contents.size(); i++) {
		if (i != 0)
			ss << ", ";
		// Do a dynamic lookup of the value.
		// TODO: Lookup and call here.
//		ClFunction*
//		ss << 
	}
	ss << "]";
	result->contents = ss.str();
	return result;
}

ClObj* cl_builtin_list_append(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(1)
	ClList* this_list = assert_kind<ClList>(this_function->closed_this);
	this_list->contents.push_back(arguments[0]);
	// Must double increment reference when returning.
	// Once because we've stored the object in the list, and once because we're returning it.
	arguments[0]->inc_ref();
	arguments[0]->inc_ref();
	return arguments[0];
}

ClObj* cl_builtin_list_iter(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
//	ClList* this_list = assert_kind<ClList>(this_function->closed_this);
	// Build the closure.
	static const string* staticstring_listiterator = new string("listiterator");
	ClFunction* f = this_function->parent->create_function(
		0,
		false,
		staticstring_listiterator,
		nullptr
	);
	f->native_executable_content = cl_builtin_list_iterator;
	f->closed_this = this_function->closed_this;
	f->closed_this->inc_ref();
	f->cache_as<uint64_t>() = 0;
	return f;
}

ClObj* cl_builtin_list_iterator(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
//	cout << "List iterator is being called." << endl;
	ClList* this_list = assert_kind<ClList>(this_function->closed_this);
	// We interpret the native cache as an int64_t, which we use as an index into the list.
	uint64_t& iteration_index = *reinterpret_cast<uint64_t*>(&this_function->native_executable_cache);

	// If we're done iterating, then note so.
	if (iteration_index >= this_list->contents.size()) {
		ClStopIteration* stop_iteration = this_function->parent->stop_iteration;
		stop_iteration->inc_ref();
		return stop_iteration;
	}

	// Otherwise, return the next value in line.
	ClObj* obj = this_list->contents[iteration_index++];
//	cout << "This closed list: " << this_list << " " << this_list->ref_count << endl;
//	cout << "Returning this guy: " << obj << " " << obj->ref_count << endl;
	obj->inc_ref();
	return obj;
}

#if 0
ClObj* cl_builtin_dict_iter(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	static const string* staticstring_dictiterator = new string("dictiterator");
	ClFunction* f = this_function->parent->create_function(
		0,
		false,
		staticstring_dictiterator,
		nullptr
	);
	f->native_executable_content = cl_builtin_dict_iterator;
	f->closed_this = this_function->closed_this;
	f->closed_this->inc_ref();
	f->cache_as<uint64_t>() = 0;
	return f;
}

ClObj* cl_builtin_dict_iterator(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	ClDict* this_dict = assert_kind<ClDict>(this_function->closed_this);
}
#endif

ClObj* cl_builtin_len(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(1)
	ClObj* argument = arguments[0];
	ClDataContext* data_ctx = this_function->parent;
	cl_int_t value;
	switch (argument->kind) {
		case (CL_LIST): {
			value = static_cast<ClList*>(argument)->contents.size();
			break;
		}
		case (CL_STRING): {
			value = static_cast<ClString*>(argument)->contents.size();
			break;
		}
		case (CL_INSTANCE): {
			data_ctx->traceback_and_crash("Currently __len__ dispatch is unimplemented.");
			break;
		}
		default:
			data_ctx->traceback_and_crash("Type error on len.");
			return nullptr; // Suppress compiler warning about no-return.
	}
	ClInt* result = data_ctx->create<ClInt>();
	result->value = value;
	return result;
}

ClObj* cl_builtin_methodify(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(1)
	ClFunction* func = assert_kind<ClFunction>(arguments[0]);
	func->is_method = true;
	func->inc_ref();
	return func;
}

ClObj* cl_builtin_getparent(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(1)
	ClInstance* instance = assert_kind<ClInstance>(arguments[0]);
	if (instance->scope_parent == nullptr) {
		this_function->parent->nil->inc_ref();
		return this_function->parent->nil;
	}
	instance->scope_parent->inc_ref();
	return instance->scope_parent;
}

ClObj* cl_builtin_getkind(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(1)
	ClString* kind_string = this_function->parent->create<ClString>();
	kind_string->contents = cl_kind_to_name[arguments[0]->kind];
	return kind_string;
}

ClObj* cl_builtin_upto(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(1)
	ClDataContext* data_ctx = this_function->parent;
	cl_int_t count_upto_argument = assert_kind<ClInt>(arguments[0])->value;
	ClInstance* result = data_ctx->create<ClInstance>();
	// Make the new result instance inherit from our closed this.
	result->scope_parent = assert_kind<ClInstance>(this_function->closed_this);
	result->scope_parent->inc_ref();
	ClInt* counter = data_ctx->create<ClInt>();
	counter->value = 0;
	ClInt* upto = data_ctx->create<ClInt>();
	upto->value = count_upto_argument;
	result->table["i"] = counter;
	result->table["upto"] = upto;
	return result;
}

ClObj* cl_builtin_upto_iterator(ClFunction* this_function, int argument_count, ClObj** arguments);

ClObj* cl_builtin_upto_base_iter(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	static const string* staticstring_uptoiterator = new string("uptoiterator");
	ClFunction* f = this_function->parent->create_function(
		0,
		false,
		staticstring_uptoiterator,
		nullptr
	);
	f->native_executable_content = cl_builtin_upto_iterator;
	f->closed_this = this_function->closed_this;
	f->closed_this->inc_ref();
	return f;
}

ClObj* cl_builtin_upto_iterator(ClFunction* this_function, int argument_count, ClObj** arguments) {
	ASSERT_ARGUMENT_COUNT(0)
	ClDataContext* data_ctx = this_function->parent;
	ClInstance& this_instance = *assert_kind<ClInstance>(this_function->closed_this);
	// We now do some unsafe manipulations for efficiency.
	// If our closed instances are mutated in illegal ways then this could fail horribly.
	ClInt* i = assert_kind<ClInt>(this_instance.table["i"]);
	ClInt* upto = assert_kind<ClInt>(this_instance.table["upto"]);
	if (i->value < upto->value) {
		// We're going to return i, so increment it before we assign over it in this_instance's table, to avoid freeing it accidentally.
		i->inc_ref();
		ClInt* next_counter_value = data_ctx->create<ClInt>();
		next_counter_value->value = i->value + 1;
		cl_store_to_object_table(&this_instance, next_counter_value, "i");
		// Decrement the reference count, because it just got incremented by being inserted into the table.
		next_counter_value->dec_ref();
		return i;
	}
	// Otherwise, stop iteration.
	ClStopIteration* stop_iteration = data_ctx->stop_iteration;
	stop_iteration->inc_ref();
	return stop_iteration;
}

