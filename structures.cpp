// Data structures.

#include "structures.h"
#include <fstream>
using namespace std;

std::ostream& operator << (std::ostream& os, const ClObj& obj) {
	obj.pprint(os);
//	os << "@" << obj.ref_count;
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

// ===== ClNil =====

void ClNil::pprint(ostream& os) const {
	os << "Nil";
}

// ===== ClInt =====

void ClInt::pprint(ostream& os) const {
//	os << "Int(" << value << ")";
	os << value;
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
	// Must inc value ref first, in case we're storing over itself.
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

// ===== ClDict =====

bool DictHashEntry::operator == (const DictHashEntry& other) const {
	// Objects of different kinds are never keyqual.
	if (contents->kind != other.contents->kind)
		return false;
	switch (contents->kind) {
		case (CL_NIL):
			return true;
		case (CL_INT):
			return static_cast<ClInt*>(contents)->value == static_cast<ClInt*>(other.contents)->value;
		case (CL_BOOL):
			return static_cast<ClBool*>(contents)->truth_value == static_cast<ClBool*>(other.contents)->truth_value;
		case (CL_STRING):
			return static_cast<ClString*>(contents)->contents == static_cast<ClString*>(other.contents)->contents;
		// The following types are keyqual when they're isqual.
		case (CL_LIST):
		case (CL_RECORD):
		case (CL_DICT):
		case (CL_FUNCTION):
		case (CL_INSTANCE):
			// XXX: Do I really want to even allow mutable types (list, records, dicts) to be keyqual to others?
			return contents == other.contents;
		default:
			cout << cl_kind_to_name[contents->kind] << endl;
			cl_crash("BUG BUG BUG: Unhandled case in DictHashEntry::operator ==.");
	}
}

size_t std::hash<DictHashEntry>::operator () (const DictHashEntry& entry) const {
	return 3;
}

ClDict::~ClDict() {
	for (auto& pair : mapping) {
		pair.first.contents->dec_ref();
		pair.second->dec_ref();
	}
}

ClObj* ClDict::lookup(ClObj* key) {
	DictHashEntry entry({key});
	auto it = mapping.find(entry);
	if (it == mapping.end())
		parent->traceback_and_crash("Key error on dictionary lookup.");
	return it->second;
}

void ClDict::assign(ClObj* key, ClObj* value) {
	DictHashEntry entry({key});
	// Increment reference counts.
	key->inc_ref();
	value->inc_ref();
	// Check if we're overwriting a previous entry, and if so, decrement ref counts.
	auto it = mapping.find(entry);
	if (it != mapping.end()) {
		it->first.contents->dec_ref();
		it->second->dec_ref();
	}
	mapping[entry] = value;
}

void ClDict::pprint(ostream& os) const {
	os << "Dict";
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
	auto bound_method = parent->create_function(
		argument_count,
		false,
		function_name,
		source_file_path
	);
	bound_method->executable_content = executable_content;
//	bound_method->argument_count = argument_count;
	bound_method->closure = closure;
	// We set is_method to false, to indicate that further pulling from an object shouldn't produce method binding.
//	bound_method->is_method = false;
	if (closure != nullptr)
		closure->inc_ref();
	bound_method->closed_this = object_who_has_method;
	bound_method->native_executable_content = native_executable_content;
	bound_method->native_executable_cache = native_executable_cache;
//	bound_method->function_name = function_name;
//	bound_method->source_file_path = source_file_path;

	object_who_has_method->inc_ref();
	parent->register_object(bound_method);
	return bound_method;
}

// ===== ClInstance =====

ClInstance::~ClInstance() {
	for (auto& pair : table)
		pair.second->dec_ref();
	if (scope_parent != nullptr)
		scope_parent->dec_ref();
}

void ClInstance::pprint(ostream& os) const {
//	os << "Instance";
	os << "{";
	for (auto& pair : table)
		os << pair.first << ",";
	os << "}";
}

// ===== ClStopIteration =====

void ClStopIteration::pprint(ostream& os) const {
	os << "StopIteration (There should be no non-debugging way to print this object.)";
}

// ===== ClDataContext =====

ClDataContext::ClDataContext() : objects_registered(0), objects_freed(0) {
	// Statically allocate various objects.
	nil = new ClNil();
	register_permanent_object(nil);

	static_booleans[0] = new ClBool();
	static_booleans[0]->truth_value = false;
	register_permanent_object(static_booleans[0]);

	static_booleans[1] = new ClBool();
	static_booleans[1]->truth_value = true;
	register_permanent_object(static_booleans[1]);

	stop_iteration = new ClStopIteration();
	register_permanent_object(stop_iteration);

	// Statically allocate the global scope.
	global_scope = new ClInstance();
	register_permanent_object(global_scope);

	// We now build an array of default lookup tables for our default types.
	default_type_tables = new unordered_map<string, ClObj*>[CL_KIND_COUNT];
}

ClDataContext::~ClDataContext() {
	delete[] default_type_tables;
	for (string* s : permanent_strings)
		delete s;
}

void ClDataContext::unref_all_permanent_objects() {
	// First we decrement references on every object registered with register_permanent_object.
	for (ClObj* permanent_object : permanent_objects)
		permanent_object->dec_ref();
	// Clear out the list of permanent objects, so this method is idempotent.
	permanent_objects.clear();
	// Next we clear out the default type tables for the various kinds.
	for (int kind = 0; kind < CL_KIND_COUNT; kind++) {
		for (auto& pair : default_type_tables[kind])
			pair.second->dec_ref();
		// Clear out the unordered_map, so this method is idempotent.
		default_type_tables[kind].clear();
	}
}

ClObj* ClDataContext::register_object(ClObj* obj) {
	obj->parent = this;
	objects.insert(obj);
	objects_registered++;
	return obj;
}

ClObj* ClDataContext::register_permanent_object(ClObj* obj) {
	register_object(obj);
	permanent_objects.push_back(obj);
	return obj;
}

const string* ClDataContext::register_permanent_string(string s) {
	string* new_s = new string(s);
	permanent_strings.push_back(new_s);
	return new_s;
}

// Here we are not re-entrant, because it is assumed that we are about to crash.
static void print_line_from_file(string path, int line_number) {
	ifstream f(path);
	string line;
	for (int i = 0; i < line_number; i++)
		if (not getline(f, line))
			return;
	cout << "    " << line << endl;
}

void ClDataContext::traceback_and_crash(string message) {
	cerr << "Traceback:" << endl;
	for (ClTracebackEntry& entry : traceback) {
		if (entry.function_name == nullptr)
			cerr << "  Null pointer in traceback." << endl;
		else {
			cerr << "  " << *entry.function_name;
			if (entry.source_file_path == nullptr)
				cerr << " (builtin)" << endl;
			else
				cerr << " from \"" << *entry.source_file_path << "\", line " << entry.line_number << endl;
		}
		// TODO: Implement reading the actual lines from the files.
		if (entry.source_file_path != nullptr and entry.line_number > 0)
			print_line_from_file(*entry.source_file_path, entry.line_number);
	}
	cerr << "Error: " << message << endl;
	exit(1);
}

ClFunction* ClDataContext::create_function(int argument_count, bool is_method, const std::string* function_name, const std::string* source_file_path) {
	ClFunction* obj = new ClFunction(argument_count, is_method, function_name, source_file_path);
	register_object(obj);
	return obj;
}

ClFunction* ClDataContext::create_return_thunk(ClObj* to_return, const std::string* function_name, const std::string* source_file_path) {
	ClFunction* thunk = create_function(0, false, function_name, source_file_path);
	// Stash the object to return into the function as the closed_this parameter.
	// I'm not super happy about this, but oh well...
	thunk->closed_this = to_return;
	to_return->inc_ref();
	thunk->native_executable_content = cl_return_thunk_executable_content;
	return thunk;
}

ClObj* cl_return_thunk_executable_content(ClFunction* this_function, int argument_count, ClObj** arguments) {
	this_function->closed_this->inc_ref();
	return this_function->closed_this;
}

