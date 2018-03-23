#include "util.h"

SCM arr_to_list (int counter, SCM* arr) {
	if (counter == 1)
		return SCM_EOL;
	else
		return scm_cons (*arr, arr_to_list (counter - 1, arr + 1));
}

/*
 * I just don't understand scm_for_each
 */
void my_for_each (SCM func, SCM list) {
	SCM nlist = list;
	while (! scm_is_null (nlist)) {
		scm_call_1 (func, scm_car (nlist));
		nlist = scm_cdr (nlist);
	}
}

void my_for_each_1 (SCM func, SCM item, SCM list) {
	SCM nlist = list;
	while (! scm_is_null (nlist)) {
		scm_call_2 (func, scm_car (nlist), item);
		nlist = scm_cdr (nlist);
	}
}

/*
 * C binding of (oop goops describe)'s describe
 */
SCM scm_describe (SCM obj) {
	scm_call_1
		( scm_c_public_ref ("oop goops describe", "describe"),
		  obj );
	return SCM_UNSPECIFIED;
}
