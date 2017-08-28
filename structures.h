// Data structures.

#ifndef _CL_STRUCTURES_H
#define _CL_STRUCTURES_H

#include <cstdint>
#include <ostream>
#include <iostream>
#include <unordered_set>
#include <vector>
#include <list>
#include <string>
#include <unordered_map>

// Forward declarations for the cyclic include of cl.h.
// Unfortunately, standard C++ disallows forward declaration of enums.
// Thus, this entire enum must be included here before the cyclic include.
enum ClKind {
	CL_NIL,
	CL_INT,
	CL_BOOL,
	CL_LIST,
	CL_RECORD,
	CL_MAP,
	CL_STRING,
	CL_FUNCTION,
	CL_INSTANCE,
	CL_STOP_ITERATION,
	CL_KIND_COUNT,
};

class ClObj;
class ClRecord;
class ClFunction;
class ClInstance;
class ClDataContext;
typedef int64_t cl_int_t;

// Do the cyclic include.
#include "cl.h"

const char* const cl_kind_to_name[CL_KIND_COUNT] = {
	"Nil",
	"Int",
	"Bool",
	"List",
	"Record",
	"Map",
	"String",
	"Function",
	"Instance",
	"StopIteration",
};

struct ClObj {
	ClDataContext* parent;
	int ref_count;
	ClKind kind;

	void dec_ref();
	void inc_ref();

	ClObj(ClKind kind) : ref_count(1), kind(kind) {}

	virtual ~ClObj();
	bool test_kind(ClKind kind);
	virtual void pprint(std::ostream& os) const = 0;
};

std::ostream& operator << (std::ostream& os, const ClObj& obj);

struct ClNil : public ClObj {
	ClNil() : ClObj(CL_NIL) {}
	virtual void pprint(std::ostream& os) const;
};

struct ClInt : public ClObj {
	cl_int_t value;

	ClInt() : ClObj(CL_INT) {}
	virtual void pprint(std::ostream& os) const;
};

struct ClBool : public ClObj {
	bool truth_value;

	ClBool() : ClObj(CL_BOOL) {}
	virtual void pprint(std::ostream& os) const;
};

struct ClList : public ClObj {
	std::vector<ClObj*> contents;

	ClList() : ClObj(CL_LIST) {}
	virtual ~ClList();
	virtual void pprint(std::ostream& os) const;
};

struct ClRecord : public ClObj {
	// NB: If you add members here, patch up ClRecord::duplicate to copy them over!
	// XXX: TODO: Make a decision as to what type to use for these. Probably just plain int?
	cl_int_t distinguisher;
	cl_int_t length;
	ClObj** contents;

	ClRecord(cl_int_t distinguisher, cl_int_t length, ClObj* fill=nullptr);
	virtual ~ClRecord();
	virtual void pprint(std::ostream& os) const;
	ClObj* load(int index) const;
	void store(int index, ClObj* value);

	// When you duplicate, you are responsible for setting the ref count on the duplicate and registering it, if you want to keep it around.
	ClRecord* duplicate() const;
};

struct ClMap : public ClObj {
	std::unordered_map<cl_int_t, ClObj*> mapping;

	ClMap() : ClObj(CL_MAP) {}
	virtual ~ClMap();
	virtual void pprint(std::ostream& os) const;
};

struct ClString : public ClObj {
	std::string contents;

	ClString() : ClObj(CL_STRING) {}
	virtual void pprint(std::ostream& os) const;
};

struct ClFunction : public ClObj {
	// NB: If you add members here, patch up ClFunction::produce_bound_method to copy them over!
	int argument_count;
	// Every function consists of an executable content...
	ClInstructionSequence* executable_content = nullptr;
	// ... and a record for the local closure.
	ClRecord* closure = nullptr;
	// Finally, when pulling out methods we generate a closed_this, forming a bound method.
	bool is_method = false;
	ClObj* closed_this = nullptr;
	// Function pointer for implementing native functions.
	ClObj* (*native_executable_content)(ClFunction* this_function, int argument_count, ClObj** arguments) = nullptr;
	void* native_executable_cache = nullptr;
	const std::string* function_name;
	const std::string* source_file_path;

	template <typename T>
	inline T& cache_as() {
		return *reinterpret_cast<T*>(&native_executable_cache);
	}

	virtual ~ClFunction();
	ClFunction(
		int argument_count,
		bool is_method,
		const std::string* function_name,
		const std::string* source_file_path
	) : ClObj(CL_FUNCTION),
	    argument_count(argument_count),
	    is_method(is_method),
	    function_name(function_name),
	    source_file_path(source_file_path) {}
	virtual void pprint(std::ostream& os) const;
	ClFunction* produce_bound_method(ClObj* object_who_has_method);
};

struct ClInstance : public ClObj {
	std::unordered_map<std::string, ClObj*> table;
	ClInstance* scope_parent = nullptr;

	ClInstance() : ClObj(CL_INSTANCE) {}
	virtual ~ClInstance();
	virtual void pprint(std::ostream& os) const;
};

struct ClStopIteration : public ClObj {
	ClStopIteration() : ClObj(CL_STOP_ITERATION) {}
	virtual void pprint(std::ostream& os) const;
};

struct ClTracebackEntry {
	const std::string* function_name;
	const std::string* source_file_path;
	int line_number;
};

struct ClDataContext {
	std::unordered_set<ClObj*> objects;
	std::vector<ClObj*> permanent_objects;
	ClNil* nil;
	ClBool* static_booleans[2];
	ClStopIteration* stop_iteration;
	ClInstance* global_scope;
	int64_t objects_registered;
	int64_t objects_freed;
	std::vector<std::string*> permanent_strings;

	std::unordered_map<std::string, ClObj*>* default_type_tables;
	std::list<ClTracebackEntry> traceback;

	ClDataContext();
	~ClDataContext();
	void unref_all_permanent_objects();
	// In order to be tracked by the garbage collector, you must call register_object on each new ClObj allocated.
	ClObj* register_object(ClObj* obj);
	// If you don't want garbage collection, and thus don't want a ``leaked object'' warning at exit, register with this. 
	ClObj* register_permanent_object(ClObj* obj);
	// This routine is just for producing strings that we want to free later.
	const std::string* register_permanent_string(std::string s);

	template <typename T> T* create();
	ClFunction* create_function(int argument_count, bool is_method, const std::string* function_name, const std::string* source_file_path);
	ClFunction* create_return_thunk(ClObj* to_return, const std::string* function_name, const std::string* source_file_path);

	void traceback_and_crash(std::string message) __attribute__ ((noreturn));
};

namespace cl_template_trickery {
	// Here we use a specialized template to allow users to look up the corresponding ClKind to a given ClObj subclass.
	template <typename T> struct get_kind {};

	template <> struct get_kind<ClNil>           { constexpr static ClKind kind = CL_NIL; };
	template <> struct get_kind<ClInt>           { constexpr static ClKind kind = CL_INT; };
	template <> struct get_kind<ClBool>          { constexpr static ClKind kind = CL_BOOL; };
	template <> struct get_kind<ClList>          { constexpr static ClKind kind = CL_LIST; };
	template <> struct get_kind<ClRecord>        { constexpr static ClKind kind = CL_RECORD; };
	template <> struct get_kind<ClMap>           { constexpr static ClKind kind = CL_MAP; };
	template <> struct get_kind<ClString>        { constexpr static ClKind kind = CL_STRING; };
	template <> struct get_kind<ClFunction>      { constexpr static ClKind kind = CL_FUNCTION; };
	template <> struct get_kind<ClInstance>      { constexpr static ClKind kind = CL_INSTANCE; };
	template <> struct get_kind<ClStopIteration> { constexpr static ClKind kind = CL_STOP_ITERATION; };
}

template <typename T>
static inline T* assert_kind(ClObj* obj) {
	if (obj == nullptr) {
		cl_crash("nullptr in assert_kind!");
	}
	if (obj->ref_count <= 0) {
		cl_crash("Non-positive ref count on used object!");
	}
	if (obj->kind != cl_template_trickery::get_kind<T>::kind) {
		std::string error_message = "Type error, expected a ";
		error_message += std::string(cl_kind_to_name[cl_template_trickery::get_kind<T>::kind]);
		error_message += ", instead got: ";
		error_message += cl_kind_to_name[obj->kind];
		obj->parent->traceback_and_crash(error_message);
	};
	return static_cast<T*>(obj);
}

template <typename T>
T* ClDataContext::create() {
	T* obj = new T();
	register_object(obj);
	return obj;
}

ClObj* cl_return_thunk_executable_content(ClFunction* this_function, int argument_count, ClObj** arguments);

#endif

