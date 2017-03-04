// Data structures.

#ifndef _CL_STRUCTURES_H
#define _CL_STRUCTURES_H

#include <iostream>
#include <ostream>
#include <unordered_set>
#include <map>
#include <string>
#include <stdint.h>

// Forward declarations for the cyclic include of cl.h.
class ClObj;
class ClRecord;
class ClDataContext;
typedef int64_t cl_int_t;

#include "cl.h"

enum ClKind {
	CL_NIL,
	CL_INT,
	CL_CONS,
	CL_RECORD,
	CL_MAP,
	CL_STRING,
	CL_FUNCTION,
	CL_INVALID,
};

struct ClObj {
	ClDataContext* parent;
	int ref_count;
	ClKind kind;

	void dec_ref();
	void inc_ref();

	ClObj(ClKind kind) : kind(kind) {}

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

struct ClCons : public ClObj {
	ClObj* head;
	ClCons* tail;

	ClCons() : ClObj(CL_CONS) {}
	virtual ~ClCons();
	virtual void pprint(std::ostream& os) const;
};

struct ClRecord : public ClObj {
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
	std::map<cl_int_t, ClObj*> mapping;

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
	// Every function consists of an executable content...
	ClInstructionSequence* executable_content;
	// ... and a record for the local closure.
	ClRecord* closure;

	ClFunction() : ClObj(CL_FUNCTION) {}
	virtual void pprint(std::ostream& os) const;
};

struct ClDataContext {
	std::unordered_set<ClObj*> objects;
	ClNil* nil;

	ClDataContext();
	ClObj* register_object(ClObj* obj);
};

#endif

