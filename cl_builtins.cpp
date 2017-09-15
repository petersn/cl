// Built-in classes and methods.

using namespace std;
#include <sstream>
#include <cassert>
#include "cl.h"
#include "cl_builtins.h"

// XXX: Until I find a work-around, or C++20 fixes this, I'll resort to ugly crap like this.
#define CREATOR_SKIP \
	.func_argument_count = 0, \
	.func_is_method = false, \
	.func = nullptr, \
	.string_contents = "",

#define INSTANCE_SKIP \
	CREATOR_SKIP \
	.creator = nullptr,

vector<ClassBuilderEntry> main_entries = {
	{
		.name = "nil",
		CREATOR_SKIP
		.creator = [](ClContext* ctx) -> ClObj* {
			ctx->data_ctx->nil->inc_ref();
			return ctx->data_ctx->nil;
		},
	},
	{
		.name = "False",
		CREATOR_SKIP
		.creator = [](ClContext* ctx) -> ClObj* {
			ctx->data_ctx->static_booleans[0]->inc_ref();
			return ctx->data_ctx->static_booleans[0];
		},
	},
	{
		.name = "True",
		CREATOR_SKIP
		.creator = [](ClContext* ctx) -> ClObj* {
			ctx->data_ctx->static_booleans[1]->inc_ref();
			return ctx->data_ctx->static_booleans[1];
		},
	},

	// Methods of Nil
	{
		.name = "Nil:str",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			ClString* result = this_function->parent->create<ClString>();
			result->contents = "nil";
			return result;
		},
	},

	// Methods of list
	{
		.name = "List:iter",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			ClFunction* f = assert_kind<ClFunction>(cl_lookup_in_object_table(this_function->closed_this, "final_iter", true));
			// Zero out the cache.
			f->cache_as<uint64_t>() = 0;
			return f;
		},
	},
	{
		.name = "List:final_iter",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			ClList* this_list = assert_kind<ClList>(this_function->closed_this);
			// We interpret the native cache as an int64_t, which we use as an index into the list.
			auto& iteration_index = this_function->cache_as<uint64_t>();
			// If we're done iterating, then note so.
			if (iteration_index >= this_list->contents.size()) {
				ClStopIteration* stop_iteration = this_function->parent->stop_iteration;
				stop_iteration->inc_ref();
				return stop_iteration;
			}
			// Otherwise, return the next value in line.
			ClObj* obj = this_list->contents[iteration_index++];
			obj->inc_ref();
			return obj;
		},
	},

	// Methods of dict
	{
		.name = "Dict:iter",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			ClFunction* f = assert_kind<ClFunction>(cl_lookup_in_object_table(this_function->closed_this, "final_iter", true));
			ClDict* this_dict = assert_kind<ClDict>(this_function->closed_this);
			f->cache_as<decltype(this_dict->mapping)::const_iterator>() = this_dict->mapping.begin();
			// Imbued f with a closure over a mutation lock on the dictionary to guarantee non-mutation.
			ClMutationLock* lock = this_function->parent->create_mutation_lock(this_dict);
			f->closure = this_function->parent->create_record(0, 1, this_function->parent->nil);
			f->closure->store(0, lock);
			lock->dec_ref();
			return f;
		},
	},
	{
		.name = "Dict:final_iter",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			ClDict* this_dict = assert_kind<ClDict>(this_function->closed_this);
			auto& it = this_function->cache_as<decltype(this_dict->mapping)::const_iterator>();
			// If we're done iterating, then note so.
			if (it == this_dict->mapping.end()) {
				ClStopIteration* stop_iteration = this_function->parent->stop_iteration;
				stop_iteration->inc_ref();
				return stop_iteration;
			}
			// Otherwise, return the next value in line.
			ClObj* obj = (*it++).first.contents;
			obj->inc_ref();
			return obj;
		},
	},

	// upto
	{
		.name = "upto",
		INSTANCE_SKIP
		.is_instance = true,
	},
	{
		.name = "upto.construct",
		.func_argument_count = 1,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			assert_kind<ClInt>(arguments[0]);
			ClInt* zero = this_function->parent->create<ClInt>();
			zero->value = 0;
			cl_store_to_object_table(this_function->closed_this, zero, "i");
			cl_store_to_object_table(this_function->closed_this, arguments[0], "end");
			zero->dec_ref();
			this_function->parent->nil->inc_ref();
			return this_function->parent->nil;
		},
	},
	{
		.name = "upto.iter",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			return cl_lookup_in_object_table(this_function->closed_this, "final_iter", true);
		},
	},
	{
		.name = "upto.final_iter",
		.func_argument_count = 0,
		.func_is_method = true,
		.func = [](ClFunction* this_function, int argument_count, ClObj** arguments) -> ClObj* {
			ClDataContext* data_ctx = this_function->parent;
			ClInstance& this_instance = *assert_kind<ClInstance>(this_function->closed_this);
			// We now do some unsafe manipulations for efficiency.
			// If our closed instances are mutated in illegal ways then this could fail horribly.
			ClInt* i = assert_kind<ClInt>(this_instance.table["i"]);
			ClInt* upto = assert_kind<ClInt>(this_instance.table["end"]);
			if (i->value < upto->value) {
				// We're going to return i, so increment it before we assign over it in this_instance's table, to avoid freeing it accidentally.
				i->inc_ref();
				ClInt* next_counter_value = data_ctx->create<ClInt>();
				next_counter_value->value = i->value + 1;
				cl_store_to_object_table(&this_instance, next_counter_value, "i");
				// Decrement the reference count, because it just got incremented by being inserted into the table.
				next_counter_value->dec_ref();
				return i;
			}
			// Otherwise, stop iteration.
			ClStopIteration* stop_iteration = data_ctx->stop_iteration;
			stop_iteration->inc_ref();
			return stop_iteration;
		},
	},
};

// Lifted from https://stackoverflow.com/questions/10058606/splitting-a-string-by-a-character
static vector<string> split_by_char(string s, char c) {
	stringstream stream(s);
	string segment;
	vector<string> result;
	while (std::getline(stream, segment, c))
		result.push_back(segment);
	return result;
}

// TODO: XXX: XXX: XXX
// Make the comments on this function make sense wrt the comments below.
static ClObj* lookup_by_name(ClContext* ctx, const vector<string>& name_components) {
	ClObj* scope = ctx->data_ctx->global_scope;
	scope->inc_ref(); // (*)
	// Perform lookup into scope based on each name component except the last.
	for (const string& component : name_components) {
		ClObj* old_scope = scope; // (**)
		scope = cl_lookup_in_object_table(scope, component, false);
		old_scope->dec_ref(); // (***)
	}
	return scope;
}

void build_into_context_from_entry(ClContext* ctx, const ClassBuilderEntry& entry) {
	vector<string> name_components = split_by_char(entry.name, '.');
	assert(name_components.size() >= 1); // Name must be non-empty!
//	cout << "Components:" << endl;
//	for (auto s : name_components)
//		cout << "    Component: '" << s << "'" << endl;

	string final_symbol_name = name_components.back();

	// === Creation phase.
	// We start by building the object we're going to assign somewhere.
	ClObj* built_object = nullptr;
	if (entry.func != nullptr) {
		ClFunction* f = ctx->data_ctx->create_function(
			entry.func_argument_count,
			entry.func_is_method,
			ctx->data_ctx->register_permanent_string(final_symbol_name),
			nullptr
		);
		f->native_executable_content = entry.func;
		built_object = f;
	} else if (entry.string_contents != "") {
		ClString* s = ctx->data_ctx->create<ClString>();
		s->contents = entry.string_contents;
		built_object = s;
	} else if (entry.creator != nullptr) {
		built_object = entry.creator(ctx);
	} else if (entry.is_instance) {
		ClInstance* inst = ctx->data_ctx->create<ClInstance>();
		inst->scope_parent = assert_kind<ClInstance>(lookup_by_name(ctx, split_by_char(entry.instance_parent, '.')));
		built_object = inst;
	}
	assert(built_object != nullptr); // ClassBuilderEntry yields no object!

	// === Assignment phase.
	// Now that we've built the object to assign we need to figure out where to put it, and actually put it there.
	// There are two main cases.
	//   1) entry.name is of the form "foo.bar.baz"
	//      In this case we lookup foo.bar, then assign into its baz
	//      This is done by setting scope to the global scope then looking up all but the last name component, and finally assigning based on the final name component.
	//   2) entry.name is of the form "foo:bar"
	//      In this case foo is a builtin type table, and we assign into its bar entry.
	// In case (1), we have to be REALLY careful about reference counting, because cl_lookup_in_object_table increments reference counts, and can return brand new objects.
	// In order to make sure we get it right, we obey the protocol that the variable `scope` counts as a reference.
	// Thus, we have to increment a reference on the line. (*)
	// Further, when we overwrite the value of scope on (**) we have to decrement the reference on line (***).
	// Finally, we have to decrement the reference one last time when `scope` goes out of scope on line (****).
	// Sorry, this is somewhat complicated, but it is necessary to cope with the semantics of cl_lookup_in_object_table which can do crazy stuff, like generate objects on the fly that we might need to free.

	ClObj* scope = ctx->data_ctx->global_scope;
	scope->inc_ref(); // (*)
	// Perform lookup into scope based on each name component except the last.
	for (size_t i = 0; i < name_components.size() - 1; i++) {
		ClObj* old_scope = scope; // (**)
		scope = cl_lookup_in_object_table(scope, name_components[i], false);
		old_scope->dec_ref(); // (***)
	}

	// Finally, assign based on the last name component.
	// Here we split base on cases (1) and (2) above.
	if (final_symbol_name.find(":") != std::string::npos) {
		// == Handle case (1) -- name is of the form foo:bar 
		vector<string> split = split_by_char(final_symbol_name, ':');
		assert(split.size() == 2); // Must have exactly one colon in builtin type table assignment.
		string builtin_name = split.front();
		string symbol_name = split.back();
		assert(cl_name_to_kind.count(builtin_name) != 0); // Bad builtin name before the colon!
		ClKind kind = cl_name_to_kind[builtin_name];
		ctx->data_ctx->assign_into_default_type_table(kind, symbol_name, built_object);
	} else {
		// == Handle case (2) -- name is of the form foo.bar.baz
		cl_store_to_object_table(scope, built_object, final_symbol_name);
	}

	// Do a ref count decrement, because this assignment (of either kind) just incremented the ref count for us.
	built_object->dec_ref();
	scope->dec_ref(); // (****)
}

void build_into_context_from_entries(ClContext* ctx, const std::vector<ClassBuilderEntry>& entries) {
	for (const ClassBuilderEntry& entry : entries)
		build_into_context_from_entry(ctx, entry);
}

