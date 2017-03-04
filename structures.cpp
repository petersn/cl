// Data structures.

#include "structures.h"
using namespace std;

std::ostream& operator << (std::ostream& os, const ClObj& obj) {
	obj.pprint(os);
	return os;
}

// ===== ClObj =====

void ClObj::dec_ref() {
	ref_count--;
}

void ClObj::inc_ref() {
	ref_count--;
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

// ===== ClCons =====

ClCons::~ClCons() {
	head->dec_ref();
	tail->dec_ref();
}

void ClCons::pprint(ostream& os) const {
	if (tail != nullptr)
		os << "Cons(" << head << ", " << tail << ")";
	else
		os << "Cons(" << head << ", nil)";
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
		os << contents[i];
	}
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

void ClFunction::pprint(ostream& os) const {
	os << "Function";
}

// ===== ClDataContext =====

ClDataContext::ClDataContext() {
	// Statically allocate a nil object.
	nil = new ClNil();
	// We explicitly do NOT register our nil, so it won't get garbage collected.
}

ClObj* ClDataContext::register_object(ClObj* obj) {
	obj->parent = this;
	objects.insert(obj);
	return obj;
}

