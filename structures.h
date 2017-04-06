// Data structures.

#ifndef _CL_STRUCTURES_H
#define _CL_STRUCTURES_H

#include <stdint.h>
#include <ostream>
#include <iostream>
#include <unordered_set>
#include <vector>
#include <string>
#include <unordered_map>

// Forward declarations for the cyclic include of cl.h.
class ClObj;
class ClRecord;
class ClFunction;
class ClDataContext;
typedef int64_t cl_int_t;

#include "cl.h"

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
	// Every function consists of an executable content...
	ClInstructionSequence* executable_content = nullptr;
	// ... and a record for the local closure.
	ClRecord* closure = nullptr;
	// Finally, when pulling out methods we generate a closed_this, forming a bound method.
	bool is_method = false;
	ClObj* closed_this = nullptr;
	// Function pointer for implementing native functions.
	ClObj* (*native_executable_content)(ClFunction* this_function, ClObj* argument) = nullptr;
	void* native_executable_cache = nullptr;

	virtual ~ClFunction();
	ClFunction() : ClObj(CL_FUNCTION) {}
	virtual void pprint(std::ostream& os) const;
	ClFunction* produce_bound_method(ClObj* object_who_has_method);
};

struct ClInstance : public ClObj {
	std::unordered_map<std::string, ClObj*> table;

	ClInstance() : ClObj(CL_INSTANCE) {}
	virtual ~ClInstance();
	virtual void pprint(std::ostream& os) const;
};

struct ClStopIteration : public ClObj {
	ClStopIteration() : ClObj(CL_STOP_ITERATION) {}
	virtual void pprint(std::ostream& os) const;
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

	std::unordered_map<std::string, ClObj*>* default_type_tables;

	ClDataContext();
	void unref_all_permanent_objects();
	// In order to be tracked by the garbage collector, you must call register_object on each new ClObj allocated.
	ClObj* register_object(ClObj* obj);
	// If you don't want garbage collection, and thus don't want a ``leaked object'' warning at exit, register with this. 
	ClObj* register_permanent_object(ClObj* obj);
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
	template <> struct get_kind<ClStopIteration> { constexpr static ClKind kind = CL_STOP_ITERATION; };
}

template <typename T>
static inline T* assert_kind(ClObj* obj) {
	if (obj->kind != cl_template_trickery::get_kind<T>::kind) {
		std::string error_message = "Type error, expected a ";
		error_message += std::string(cl_kind_to_name[cl_template_trickery::get_kind<T>::kind]);
		error_message += ", instead got: ";
		error_message += cl_kind_to_name[obj->kind];
		cl_crash(error_message.c_str());
	};
	return static_cast<T*>(obj);
}

#endif

