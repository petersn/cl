// Data structures.

#ifndef _CL_STRUCTURES_H
#define _CL_STRUCTURES_H

#include <iostream>
#include <unordered_set>
#include <map>
#include <string>

typedef long long cl_int_t;

enum ClKind {
	CL_NIL,
	CL_INT,
	CL_CONS,
	CL_RECORD,
	CL_MAP,
	CL_STRING,
	CL_INVALID,
};

class ClDataContext;

struct ClObj {
	ClDataContext* parent;
	int ref_count;
	const ClKind kind = CL_INVALID;

	void dec_ref();
	void inc_ref();

	virtual ~ClObj();
	bool test_kind(ClKind kind);
};

struct ClNil : public ClObj {
	const ClKind kind = CL_NIL;
};

struct ClInt : public ClObj {
	const ClKind kind = CL_INT;
	cl_int_t value;
};

struct ClCons : public ClObj {
	const ClKind kind = CL_CONS;
	ClObj* head;
	ClCons* tail;

	virtual ~ClCons() override;
};

struct ClRecord : public ClObj {
	const ClKind kind = CL_RECORD;
	cl_int_t distinguisher;
	cl_int_t length;
	ClObj** contents;

	ClRecord(ClObj* fill, cl_int_t distinguisher, cl_int_t length);
	virtual ~ClRecord() override;
};

struct ClMap : public ClObj {
	const ClKind kind = CL_MAP;
	std::map<cl_int_t, ClObj*> mapping;

	virtual ~ClMap() override;
};

struct ClString : public ClObj {
	const ClKind kind = CL_STRING;
	std::string contents;
};

struct ClDataContext {
	std::unordered_set<ClObj*> objects;
	ClNil* nil;

	ClDataContext();
	ClObj* register_object(ClObj* obj);
};

#endif

