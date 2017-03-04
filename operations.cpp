// Various operations for Cl.
// WARNING: This file is intended to be included directly into cl.cpp, for optimization purposes.
// TODO: Maybe just use link-time optimization instead?

ClObj* ClContext::binary_plus(ClObj* left, ClObj* right) {
	// Case for int + int.
	if (left->kind == CL_INT and right->kind == CL_INT) {
		auto obj = new ClInt();
		obj->value = static_cast<ClInt*>(left)->value + static_cast<ClInt*>(right)->value;
		data_ctx->register_object(obj);
		obj->ref_count = 1;
		return obj;
	}
	// Case for string + string.
	else if (left->kind == CL_STRING and right->kind == CL_STRING) {
		auto obj = new ClString();
		obj->contents = static_cast<ClString*>(left)->contents + static_cast<ClString*>(right)->contents;
		data_ctx->register_object(obj);
		obj->ref_count = 1;
		return obj;
	}
//	// Case for cons + cons.
//	else if (left->kind == CL_CONS and right->kind == CL_CONS) {
//	}
	cl_crash("Type error on binary plus.");
}

ClObj* ClContext::binary_times(ClObj* left, ClObj* right) {
	// Case for int + int.
	if (left->kind == CL_INT and right->kind == CL_INT) {
		auto obj = new ClInt();
		obj->value = static_cast<ClInt*>(left)->value * static_cast<ClInt*>(right)->value;
		data_ctx->register_object(obj);
		obj->ref_count = 1;
		return obj;
	}
	// Case for int * string.
	else if (left->kind == CL_INT and right->kind == CL_STRING) {
		auto obj = new ClString();
		cl_int_t repetition_count = static_cast<ClInt*>(left)->value;
		const string& repeat = static_cast<ClString*>(right)->contents;
		// TODO: Replace with .reserve implementation.
    	ostringstream string_stream;
		while ((repetition_count--) > 0)
			string_stream << repeat;
		obj->contents = string_stream.str();
		data_ctx->register_object(obj);
		obj->ref_count = 1;
		return obj;
	}
	// Case for string * int.
	else if (left->kind == CL_STRING and right->kind == CL_INT) {
		return binary_times(right, left);
	}

	cl_crash("Type error on binary times.");
}

