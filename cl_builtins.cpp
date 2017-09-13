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
	}
	assert(built_object != nullptr); // ClassBuilderEntry yields no object!

	// === Assignment phase.
	// Now that we've built the object to assign we need to figure out where to put it, and actually put it there.
	// There are two main cases.
	//   1) entry.name is of the form foo.bar.baz
	//      In this case we lookup foo.bar, then assign into its baz
	//      This is done by setting scope to the global scope then looking up all but the last name component, and finally assigning based on the final name component.
	//   2) entry.name is of the form foo:bar
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

