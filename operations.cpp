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

#define Give(type) \
	auto obj = new type(); \
	data_ctx->register_object(obj);

static inline cl_int_t util_length_wrap(ClDataContext* data_ctx, cl_int_t index, cl_int_t length) {
	if (index < 0)
		index += length;
	if (index >= length)
		data_ctx->traceback_and_crash("Index out of range.");
	return index;
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

	data_ctx->traceback_and_crash("Type error on binary plus.");
}

ClObj* ClContext::binary_minus(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value - right->value;
	End_Case

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
	End_Case

	// In case of string * int or list * int, just swap the order around.
	if ((_left->kind == CL_STRING or _left->kind == CL_LIST) and _right->kind == CL_INT) {
		return binary_times(_right, _left);
	}

	data_ctx->traceback_and_crash("Type error on binary times.");
}

ClObj* ClContext::binary_divide(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value / right->value;
	End_Case

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

	data_ctx->traceback_and_crash("Type error on binary index.");
}

ClObj* ClContext::binary_in(ClObj* _left, ClObj* _right) {
	data_ctx->traceback_and_crash("Type error on binary in.");
}

ClObj* ClContext::binary_compare(ClObj* _left, ClObj* _right, ClComparisonType comparison_type) {
	Type_Case(ClInt, ClInt)
		bool truth_value;
		switch (comparison_type) {
			case (CL_COMP_EQ):                 truth_value = left->value == right->value; break;
			case (CL_COMP_NOT_EQ):             truth_value = left->value != right->value; break;
			case (CL_COMP_LESS_THAN):          truth_value = left->value <  right->value; break;
			case (CL_COMP_GREATER_THAN):       truth_value = left->value >  right->value; break;
			case (CL_COMP_LESS_THAN_OR_EQ):    truth_value = left->value <= right->value; break;
			case (CL_COMP_GREATER_THAN_OR_EQ): truth_value = left->value >= right->value; break;
			default: cl_crash("Bad comparison kind in binary compare.");
		}
		// Grab the appropriate statically allocated boolean object.
		ClObj* obj = data_ctx->static_booleans[truth_value];
		obj->inc_ref();
		return obj;
	End_Case

	data_ctx->traceback_and_crash("Type error on binary compare.");
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
		case (CL_MAP):
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

ClObj* cl_perform_function_call(ClContext* ctx, ClObj* supposed_function, ClObj* argument) {
	// If it's an instance, then calling is subclassing/instantiation.
	if (supposed_function->kind == CL_INSTANCE) {
		ClInstance* instance_obj = assert_kind<ClInstance>(supposed_function);
		ClInstance* result = supposed_function->parent->create<ClInstance>();
		result->scope_parent = instance_obj;
		instance_obj->inc_ref();
		return result;
	}
	ClFunction* function_obj = assert_kind<ClFunction>(supposed_function);
	ClObj* return_value;
	// We now do an important case check, to determine if function_obj is a native function, or Cl function.
	// If it's a Cl function, then function_obj->native_executable_content is nullptr.
	// If it's native, then function_obj->native_executable_content is a ClObj* (*)(ClObj* argument) pointing to the code.
	if (function_obj->native_executable_content != nullptr) {
		// === Native function call ===
		return_value = function_obj->native_executable_content(function_obj, argument);
	} else {
		// === Cl (non-native) function call ===
		// We duplicate the function closure to get a scope to execute in.
		// Note that we neither register nor set the ref count on this record, because we're about to throw it away.
		ClRecord* child_scope = function_obj->closure->duplicate();
		// We set the magic value 0 in the child_scope to be the passed in argument.
		child_scope->store(0, argument);
		// Do execution!
		return_value = ctx->execute(function_obj->function_name, function_obj->source_file_path, function_obj->closed_this, child_scope, function_obj->executable_content);
		// By deleting the child scope we effectively decrement the ref count on argument, completing our obligation.
		delete child_scope;
	}
	return return_value;
}

ClObj* cl_lookup_in_object_table(ClObj* object, const string& name) {
	const unordered_map<string, ClObj*>* object_table;
	if (object->kind == CL_INSTANCE)
		object_table = &static_cast<ClInstance*>(object)->table;
	else
		object_table = &object->parent->default_type_tables[object->kind];
	auto result = object_table->find(name);
	if (object->kind == CL_INSTANCE and
	    result == object_table->end() and
	    static_cast<ClInstance*>(object)->scope_parent != nullptr)
		return cl_lookup_in_object_table(static_cast<ClInstance*>(object)->scope_parent, name);
	if (result == object_table->end()) {
		string message = "No attribute \"";
		message += name;
		message += "\" found.";
		object->parent->traceback_and_crash(message);
//		// This increment is correct and necessary.
//		object->parent->nil->inc_ref();
//		return object->parent->nil;
	}
	// Otherwise, increment the reference count and return the object we got.
	ClObj* result_obj = (*result).second;
	result_obj->inc_ref();
	return result_obj;
}

void cl_store_to_object_table(ClObj* object_to_store_in, ClObj* value_to_store, const string& name) {
	if (object_to_store_in->kind != CL_INSTANCE)
		object_to_store_in->parent->traceback_and_crash("Attempt to mutate table of non-instance.");
	unordered_map<string, ClObj*>& object_table = static_cast<ClInstance*>(object_to_store_in)->table;
	// Decrement the ref count on the old object if we're overwriting.
	auto result = object_table.find(name);
	if (result != object_table.end())
		(*result).second->dec_ref();
	// Now we are free to overwrite.
	object_table[name] = value_to_store;
	value_to_store->inc_ref();
}

// ===== Built-in functions =====

ClObj* cl_builtin_nil_to_string(ClFunction* this_function, ClObj* _argument) {
	ClDataContext* data_ctx = this_function->parent;
	assert_kind<ClNil>(_argument);
	ClString* result = data_ctx->create<ClString>();
	result->contents = "nil";
	return result;
}

ClObj* cl_builtin_int_to_string(ClFunction* this_function, ClObj* _argument) {
	ClDataContext* data_ctx = this_function->parent;
	assert_kind<ClNil>(_argument);
	ClInt* argument = assert_kind<ClInt>(this_function->closed_this);
	ClString* result = data_ctx->create<ClString>();
	result->contents = to_string(argument->value);
	return result;
}

ClObj* cl_builtin_bool_to_string(ClFunction* this_function, ClObj* _argument) {
	ClDataContext* data_ctx = this_function->parent;
	assert_kind<ClNil>(_argument);
	ClBool* argument = assert_kind<ClBool>(this_function->closed_this);
	ClString* result = data_ctx->create<ClString>();
	if (argument->truth_value)
		result->contents = "True";
	else
		result->contents = "False";
	return result;
}

ClObj* cl_builtin_list_to_string(ClFunction* this_function, ClObj* _argument) {
	ClDataContext* data_ctx = this_function->parent;
	assert_kind<ClNil>(_argument);
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

ClObj* cl_builtin_list_append(ClFunction* this_function, ClObj* argument) {
	ClList* this_list = assert_kind<ClList>(this_function->closed_this);
	this_list->contents.push_back(argument);
	// Must double increment reference when returning.
	// Once because we've stored the object in the list, and once because we're returning it.
	argument->inc_ref();
	argument->inc_ref();
	return argument;
}

ClObj* cl_builtin_list_iter(ClFunction* this_function, ClObj* argument) {
	assert_kind<ClNil>(argument);
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
	obj->inc_ref();
	return obj;
}

ClObj* cl_builtin_len(ClFunction* this_function, ClObj* argument) {
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
		default:
			this_function->parent->traceback_and_crash("Type error on len.");
			return nullptr; // Suppress compiler warning about no-return.
	}
	ClInt* result = data_ctx->create<ClInt>();
	result->value = value;
	return result;
}

ClObj* cl_builtin_methodify(ClFunction* this_function, ClObj* argument) {
	ClFunction* func = assert_kind<ClFunction>(argument);
	func->is_method = true;
	func->inc_ref();
	return func;
}

ClObj* cl_builtin_upto(ClFunction* this_function, ClObj* argument) {
	ClDataContext* data_ctx = this_function->parent;
	cl_int_t count_upto_argument = assert_kind<ClInt>(argument)->value;
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

ClObj* cl_builtin_upto_base_iter(ClFunction* this_function, ClObj* argument) {
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
	ClStopIteration* stop_iteration = this_function->parent->stop_iteration;
	stop_iteration->inc_ref();
	return stop_iteration;
}

