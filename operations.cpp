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
	obj->ref_count = 1; \
	data_ctx->register_object(obj);

static cl_int_t util_length_wrap(cl_int_t index, cl_int_t length) {
	if (index < 0)
		index += length;
	if (index >= length)
		cl_crash("Index out of range.");
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
	End_Case

	cl_crash("Type error on binary plus.");
}

ClObj* ClContext::binary_minus(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value - right->value;
	End_Case

	cl_crash("Type error on binary minus.");
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

	cl_crash("Type error on binary times.");
}

ClObj* ClContext::binary_divide(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value / right->value;
	End_Case

	cl_crash("Type error on binary divide.");
}

ClObj* ClContext::binary_modulo(ClObj* _left, ClObj* _right) {
	Type_Case(ClInt, ClInt)
		Give(ClInt)
		obj->value = left->value % right->value;
		// Force the representative to be in [0, right).
		if (obj->value < 0)
			obj->value += right->value;
	End_Case

	cl_crash("Type error on binary modulo.");
}

ClObj* ClContext::binary_index(ClObj* _left, ClObj* _right) {
	Type_Case(ClString, ClInt)
		Give(ClString)
		cl_int_t index = util_length_wrap(right->value, left->contents.size());
		obj->contents = string(&left->contents[index], 1);
	End_Case

	Type_Case(ClList, ClInt)
		cl_int_t index = util_length_wrap(right->value, left->contents.size());
		ClObj* obj = left->contents[index];
		obj->inc_ref();
	End_Case

	cl_crash("Type error on binary index.");
}

ClObj* ClContext::binary_in(ClObj* _left, ClObj* _right) {
	cl_crash("Type error on binary in.");
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

	cl_crash("Type error on binary compare.");
}

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
		default:
			cl_crash("BUG BUG BUG: Unhandled case in cl_coerce_to_boolean.");
			return false; // Suppress compiler warning.
	}
}

