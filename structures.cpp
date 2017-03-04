// Data structures.

#include "structures.h"

// ===== ClObj =====

void ClObj::dec_ref() {
	ref_count--;
}

void ClObj::inc_ref() {
	ref_count--;
}

ClObj::~ClObj() {
	std::cout << "Calling parent!" << std::endl;
}

bool ClObj::test_kind(ClKind kind) {
	return this->kind == kind;
}

// ===== ClCons =====

ClCons::~ClCons() {
	head->dec_ref();
	tail->dec_ref();
}

// ===== ClRecord =====

ClRecord::ClRecord(ClObj* fill, cl_int_t distinguisher, cl_int_t length) : distinguisher(distinguisher), length(length) {
	contents = new ClObj*[length];
	for (cl_int_t i = 0; i < length; i++)
		contents[i] = fill;
	fill->ref_count += length;
}

ClRecord::~ClRecord() {
	for (cl_int_t i = 0; i < length; i++)
		contents[i]->dec_ref();
	delete[] contents;
}

// ===== ClMap =====

ClMap::~ClMap() {
	for (auto& pair : mapping)
		pair.second->dec_ref();
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

