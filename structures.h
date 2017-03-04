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
class ClDataContext;
class ClRecord;
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
	const ClKind kind = CL_INVALID;

	void dec_ref();
	void inc_ref();

	virtual ~ClObj();
	bool test_kind(ClKind kind);
	virtual void pprint(std::ostream& os) const = 0;
};

std::ostream& operator << (std::ostream& os, const ClObj& obj);

struct ClNil : public ClObj {
	const ClKind kind = CL_NIL;

	virtual void pprint(std::ostream& os) const;
};

struct ClInt : public ClObj {
	const ClKind kind = CL_INT;
	cl_int_t value;

	virtual void pprint(std::ostream& os) const;
};

struct ClCons : public ClObj {
	const ClKind kind = CL_CONS;
	ClObj* head;
	ClCons* tail;

	virtual ~ClCons();
	virtual void pprint(std::ostream& os) const;
};

struct ClRecord : public ClObj {
	const ClKind kind = CL_RECORD;
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
	const ClKind kind = CL_MAP;
	std::map<cl_int_t, ClObj*> mapping;

	virtual ~ClMap();
	virtual void pprint(std::ostream& os) const;
};

struct ClString : public ClObj {
	const ClKind kind = CL_STRING;
	std::string contents;

	virtual void pprint(std::ostream& os) const;
};

struct ClFunction : public ClObj {
	const ClKind kind = CL_FUNCTION;

	// Every function consists of an executable content...
	ClInstructionSequence* executable_content;
	// ... and a record for the local closure.
	ClRecord* closure;

	virtual void pprint(std::ostream& os) const;
};

struct ClDataContext {
	std::unordered_set<ClObj*> objects;
	ClNil* nil;

	ClDataContext();
	ClObj* register_object(ClObj* obj);
};

#endif

