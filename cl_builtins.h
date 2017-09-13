// Built-in classes and methods.

#ifndef _CL_BUILTINS_H
#define _CL_BUILTINS_H

#include <string>
#include <vector>
#include "cl.h"

typedef ClObj* (*cl_method_prototype)(ClFunction* this_function, int argument_count, ClObj** arguments);
typedef ClObj* (*cl_object_creator_prototype)(ClContext* ctx);

struct ClassBuilderEntry {
	std::string name;
	int func_argument_count;
	bool func_is_method;
	cl_method_prototype func;
	std::string string_contents;
	cl_object_creator_prototype creator;
};

void build_into_context_from_entries(ClContext* ctx, const std::vector<ClassBuilderEntry>& entries);
void build_into_context_from_entry(ClContext* ctx, const ClassBuilderEntry& entry);

extern std::vector<ClassBuilderEntry> main_entries;

#endif

