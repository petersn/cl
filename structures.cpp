// Data structures.

#include "structures.h"
using namespace std;

std::ostream& operator << (std::ostream& os, const ClObj& obj) {
	obj.pprint(os);
	os << "@" << obj.ref_count;
	return os;
}

// ===== ClObj =====

void ClObj::dec_ref() {
	ref_count--;
	if (ref_count <= 0) {
		if (ref_count < 0) {
			cout << "ERROR! Negative reference count on: kind=" << kind << " of refs=" << ref_count << endl;
			cout << "(Both that kind and ref-count should be taken with a" << endl;
			cout << "grain of salt, as they were reading a freed buffer.)" << endl;
		}
		parent->objects.erase(this);
		parent->objects_freed++;
		delete this;
	}
}

void ClObj::inc_ref() {
	ref_count++;
}

ClObj::~ClObj() {
//	cout << "Calling parent!" << endl;
}

bool ClObj::test_kind(ClKind kind) {
	return this->kind == kind;
}

// ===== ClNil =====

void ClNil::pprint(ostream& os) const {
	os << "Nil";
}

// ===== ClInt =====

void ClInt::pprint(ostream& os) const {
	os << "Int(" << value << ")";
}

// ===== ClBool =====

void ClBool::pprint(ostream& os) const {
	if (truth_value)
		os << "True";
	else
		os << "False";
}

// ===== ClList =====

ClList::~ClList() {
	for (ClObj* ptr : contents)
		ptr->dec_ref();
}

void ClList::pprint(ostream& os) const {
	os << "[";
	bool flag = false;
	for (ClObj* ptr : contents) {
		if (flag)
			os << ", ";
		os << *ptr;
		flag = true;
	}
	os << "]";
}

// ===== ClRecord =====

ClRecord::ClRecord(cl_int_t distinguisher, cl_int_t length, ClObj* fill) : ClObj(CL_RECORD), distinguisher(distinguisher), length(length) {
	contents = new ClObj*[length];
	for (cl_int_t i = 0; i < length; i++)
		contents[i] = fill;
	if (fill != nullptr)
		fill->ref_count += length;
}

ClRecord::~ClRecord() {
	for (cl_int_t i = 0; i < length; i++)
		contents[i]->dec_ref();
	delete[] contents;
}

void ClRecord::pprint(ostream& os) const {
	os << "Record" << distinguisher << "[";
	for (int i = 0; i < length; i++) {
		if (i != 0)
			os << ", ";
		os << *contents[i];
	}
	os << "]";
}

ClObj* ClRecord::load(int index) const {
	if (index < 0 or index >= length)
		cl_crash("Record load index out of range.");
	return contents[index];
}

void ClRecord::store(int index, ClObj* value) {
	if (index < 0 or index >= length)
		cl_crash("Record store index out of range.");
	value->inc_ref();
	contents[index]->dec_ref();
	contents[index] = value;
}

ClRecord* ClRecord::duplicate() const {
	auto dup = new ClRecord(distinguisher, length);
	for (int i = 0; i < length; i++) {
		dup->contents[i] = contents[i];
		contents[i]->inc_ref();
	}
	return dup;
}

// ===== ClMap =====

ClMap::~ClMap() {
	for (auto& pair : mapping)
		pair.second->dec_ref();
}

void ClMap::pprint(ostream& os) const {
	os << "Map";
}

// ===== ClString =====

void ClString::pprint(ostream& os) const {
	os << "String(\"" << contents << "\")";
}

// ===== ClFunction =====

ClFunction::~ClFunction() {
	// We need to not delete executable_context, because it is owned by the bytecode compiler, and can be shared with other ClFunctions.
	// However, our closure should have its reference decremented.
	if (closure != nullptr)
		closure->dec_ref();
	// In the current implementation this should ALWAYS cause the closure to be collected, but maybe later it'll be possible to extract this ClRecord?

	// We also need to decrement the reference count of the closed this pointer, if we're a bound method.
	if (closed_this != nullptr)
		closed_this->dec_ref();
}

void ClFunction::pprint(ostream& os) const {
	os << "Function";
}

ClFunction* ClFunction::produce_bound_method(ClObj* object_who_has_method) {
	auto bound_method = new ClFunction();
	bound_method->executable_content = executable_content;
	bound_method->closure = closure;
	// We set is_method to false, to indicate that further pulling from an object shouldn't produce method binding.
	bound_method->is_method = false;
	if (closure != nullptr)
		closure->inc_ref();
	bound_method->closed_this = object_who_has_method;
	bound_method->native_executable_content = native_executable_content;

	object_who_has_method->inc_ref();
	return bound_method;
}

// ===== ClDataContext =====

ClDataContext::ClDataContext() : objects_registered(0), objects_freed(0) {
	// Statically allocate a nil object.
	nil = new ClNil();
	register_permanent_object(nil);
	static_booleans[0] = new ClBool();
	static_booleans[0]->truth_value = false;
	register_permanent_object(static_booleans[0]);
	static_booleans[1] = new ClBool();
	static_booleans[1]->truth_value = true;
	register_permanent_object(static_booleans[1]);

	// We now build an array of default lookup tables for our default types.
	default_type_tables = new unordered_map<string, ClObj*>[CL_KIND_COUNT];
}

ClObj* ClDataContext::register_object(ClObj* obj) {
	obj->parent = this;
	objects.insert(obj);
	objects_registered++;
	return obj;
}

ClObj* ClDataContext::register_permanent_object(ClObj* obj) {
	obj->parent = this;
	return obj;
}

